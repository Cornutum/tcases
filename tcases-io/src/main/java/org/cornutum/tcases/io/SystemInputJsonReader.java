//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;
import org.cornutum.tcases.DefUtils;
import static org.cornutum.tcases.io.SystemInputJson.*;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * An {@link ISystemInputSource} that reads from an JSON document.
 *
 */
public class SystemInputJsonReader implements ISystemInputSource, Closeable
  {  
  /**
   * Creates a new SystemInputJsonReader object.
   */
  public SystemInputJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemInputJsonReader object.
   */
  public SystemInputJsonReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link SystemInputDef} instance.
   */
  public SystemInputDef getSystemInputDef()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/system-input-schema.json"));
    ProblemHandler handler = ProblemHandler.throwing();
    try( JsonReader reader = service.createReader( stream_, schema, handler))
      {
      JsonObject json;
      try
        {
        json = reader.readObject();
        }
      catch( Exception e)
        {
        throw new SystemInputException( "Invalid system input definition", e);
        }

      return asSystemInputDef( json);
      }
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ =
      stream==null
      ? System.in
      : stream;
    }

  /**
   * Returns the SystemInputDef represented by the given JSON object.
   */
  private SystemInputDef asSystemInputDef( JsonObject json)
    {
    String systemName = json.getString( SYSTEM_KEY);
    try
      {
      SystemInputDef systemInputDef = new SystemInputDef( validIdentifier( systemName));
      
      // Get system annotations
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> systemInputDef.setAnnotation( key, has.getString( key))));

      // Get system functions
      json.keySet().stream()
        .filter( key -> !(key.equals( SYSTEM_KEY) || key.equals( HAS_KEY)))
        .forEach( function -> systemInputDef.addFunctionInputDef( asFunctionInputDef( function, json.getJsonObject( function))));

      return systemInputDef;
      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( String.format( "Error defining system=%s", systemName), e);
      }
    }

  /**
   * Returns the FunctionInputDef represented by the given JSON object.
   */
  private FunctionInputDef asFunctionInputDef( String functionName, JsonObject json)
    {
    FunctionInputDef functionInputDef;

    try
      {
      functionInputDef = new FunctionInputDef( validIdentifier( functionName));
      
      // Get function annotations
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> functionInputDef.setAnnotation( key, has.getString( key))));

      // Get function variables.
      json.keySet().stream()
        .filter( key -> !key.equals( HAS_KEY))
        .forEach( varType -> getVarDefs( varType, json.getJsonObject( varType)).forEach( varDef -> functionInputDef.addVarDef( varDef)));

      if( functionInputDef.getVarTypes().length == 0)
        {
        throw new SystemInputException( String.format( "No variables defined for function=%s", functionName));
        }

      SystemInputs.getPropertiesUndefined( functionInputDef)
        .entrySet().stream()
        .findFirst()
        .ifPresent( undefined -> {
          String property = undefined.getKey();
          IConditional ref = undefined.getValue().stream().findFirst().orElse( null);
          throw new SystemInputException
            ( String.format
              ( "Property=%s is undefined, but referenced by %s",
                property,
                SystemInputs.getReferenceName( ref)));
          });

      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( String.format( "Error defining function=%s", functionName), e);
      }
    
    return functionInputDef;
    }

  /**
   * Returns the variable definitions represented by the given JSON object.
   */
  private Stream<IVarDef> getVarDefs( String varType, JsonObject json)
    {
    try
      {
      // Get annotations for this group of variables
      Annotated groupAnnotations = new Annotated(){};
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> groupAnnotations.setAnnotation( key, has.getString( key))));

      // Return variables for this variable type.
      return
        json.keySet().stream()
        .filter( key -> !key.equals( HAS_KEY))
        .map( varName -> asVarDef( varName, varType, groupAnnotations, json.getJsonObject( varName)));
      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( String.format( "Error defining variables of type=%s", varType), e);
      }
    }

  /**
   * Returns the variable definition represented by the given JSON object.
   */
  private IVarDef asVarDef( String varName, String varType, Annotated groupAnnotations, JsonObject json)
    {
    try
      {
      validIdentifier( varName);

      AbstractVarDef varDef =
        json.containsKey( MEMBERS_KEY)
        ? new VarSet( varName)
        : new VarDef( varName);

      varDef.setType( varType);
    
      // Get annotations for this variable
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> varDef.setAnnotation( key, has.getString( key))));
    
      // Get the condition for this variable
      Optional.ofNullable( json.getJsonObject( WHEN_KEY))
        .ifPresent( object -> varDef.setCondition( asValidCondition( object)));

      if( json.containsKey( MEMBERS_KEY))
        {
        VarSet varSet = (VarSet) varDef;
        getVarDefs( varType, json.getJsonObject( MEMBERS_KEY))
          .forEach( member -> varSet.addMember( member));

        if( !varSet.getMembers().hasNext())
          {
          throw new SystemInputException( String.format( "No members defined for VarSet=%s", varName));
          }
        }
      else
        {
        VarDef var = (VarDef) varDef;
        getValueDefs( json.getJsonObject( VALUES_KEY))
          .forEach( valueDef -> var.addValue( valueDef));

        if( !var.getValidValues().hasNext())
          {
          throw new SystemInputException( String.format( "No valid values defined for Var=%s", varName));
          }
        }

      // Add any group annotations
      varDef.addAnnotations( groupAnnotations);

      return varDef;
      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( String.format( "Error defining variable=%s", varName), e);
      }
    }

  /**
   * Returns the value definitions represented by the given JSON object.
   */
  private Stream<VarValueDef> getValueDefs( JsonObject json)
    {
    return
      json.keySet().stream()
      .map( valueName -> asValueDef( valueName, json.getJsonObject( valueName)));
    }

  /**
   * Returns the value definition represented by the given JSON object.
   */
  private VarValueDef asValueDef( String valueName, JsonObject json)
    {
    try
      {
      VarValueDef valueDef = new VarValueDef( ObjectUtils.toObject( valueName));

      // Get the type of this value
      boolean failure = json.getBoolean( FAILURE_KEY, false);
      boolean once = json.getBoolean( ONCE_KEY, false);
      valueDef.setType
        ( failure? VarValueDef.Type.FAILURE :
          once? VarValueDef.Type.ONCE :
          VarValueDef.Type.VALID);

      if( failure && json.containsKey( PROPERTIES_KEY))
        {
        throw new SystemInputException( "Failure type values can't define properties");
        }

      // Get annotations for this value
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> valueDef.setAnnotation( key, has.getString( key))));
    
      // Get the condition for this value
      Optional.ofNullable( json.getJsonObject( WHEN_KEY))
        .ifPresent( object -> valueDef.setCondition( asValidCondition( object)));

      // Get properties for this value
      Optional.ofNullable( json.getJsonArray( PROPERTIES_KEY))
        .map( properties -> toIdentifiers( properties)) 
        .ifPresent( properties -> valueDef.addProperties( properties)); 

      return valueDef;
      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( String.format( "Error defining value=%s", valueName), e);
      }
    }

  /**
   * Returns the condition definition represented by the given JSON object.
   */
  private ICondition asValidCondition( JsonObject json)
    {
    try
      {
      return asCondition( json);
      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( "Invalid condition", e);
      }
    }

  /**
   * Returns the condition definition represented by the given JSON object.
   */
  private ICondition asCondition( JsonObject json)
    {
    List<Function<JsonObject,Optional<ICondition>>> converters = Arrays.asList
      (
        this::asContainsAll,
        this::asContainsAny,
        this::asNot,
        this::asAnyOf,
        this::asAllOf
       );

    return
      converters.stream()
      .map( converter -> converter.apply( json))
      .filter( Optional::isPresent)
      .map( Optional::get)
      .findFirst()
      .orElse( null)
      ;
    }

  /**
   * Returns the ContainsAll condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asContainsAll( JsonObject json)
    {
    return
      json.containsKey( HAS_ALL_KEY)
      ? Optional.of( new ContainsAll( toIdentifiers( json.getJsonArray( HAS_ALL_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the ContainsAny condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asContainsAny( JsonObject json)
    {
    return
      json.containsKey( HAS_ANY_KEY)
      ? Optional.of( new ContainsAny( toIdentifiers( json.getJsonArray( HAS_ANY_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the Not condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asNot( JsonObject json)
    {
    return
      json.containsKey( NOT_KEY)
      ? Optional.of( new Not( asCondition( json.getJsonObject( NOT_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the AllOf condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asAllOf( JsonObject json)
    {
    return
      json.containsKey( ALL_OF_KEY)
      ? Optional.of( new AllOf( toConditions( json.getJsonArray( ALL_OF_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the AnyOf condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asAnyOf( JsonObject json)
    {
    return
      json.containsKey( ANY_OF_KEY)
      ? Optional.of( new AnyOf( toConditions( json.getJsonArray( ANY_OF_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the contents of the given JSON array as a list of identifiers.
   */
  private List<String> toIdentifiers( JsonArray array)
    {
    return
      IntStream.range( 0, array.size())
      .mapToObj( i -> validIdentifier( array.getString( i)))
      .collect( toList());
    }

  /**
   * Returns the contents of the given JSON array as an array of conditions.
   */
  private ICondition[] toConditions( JsonArray array)
    {
    return
      IntStream.range( 0, array.size())
      .mapToObj( i -> asCondition( array.getJsonObject( i)))
      .collect( toList())
      .toArray( new ICondition[0]);
    }

  /**
   * Reports a SystemInputException if the given string is not a valid identifier. Otherwise, returns this string.
   */
  private String validIdentifier( String string)
    {
    try
      {
      DefUtils.assertIdentifier( string);
      }
    catch( Exception e)
      {
      throw new SystemInputException( e.getMessage());
      }

    return string;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return stream_;
    }

  public void close()
    {
    IOUtils.closeQuietly( getInputStream());
    }

  private InputStream stream_;
  }
