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
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.collections4.IteratorUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonStructure;

/**
 * Converts between a {@link SystemInputDef} and its corresponding {@link JsonObject}.
 */
public final class SystemInputJson
  {
  private SystemInputJson()
    {
    // Static methods only
    }

  /**
   * Returns the JSON object that represents the given system input definition.
   */
  public static JsonObject toJson( SystemInputDef systemInput)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    builder.add( SYSTEM_KEY, systemInput.getName());

    addAnnotations( builder, systemInput);
    
    toStream( systemInput.getFunctionInputDefs()).forEach( function -> builder.add( function.getName(), toJson( function)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given function input definition.
   */
  private static JsonStructure toJson( FunctionInputDef functionInput)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, functionInput);

    Arrays.stream( functionInput.getVarTypes()).forEach( varType -> builder.add( varType, toJson( functionInput, varType)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the function variables of the given type.
   */
  private static JsonStructure toJson( FunctionInputDef functionInput, String varType)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    toStream( functionInput.getVarDefs())
      .filter( varDef -> varDef.getType().equals( varType))
      .sorted()
      .forEach( varDef -> builder.add( varDef.getName(), toJson( varDef)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given variable definition.
   */
  private static JsonStructure toJson( IVarDef varDef)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, varDef);

    ConditionJson.toJson( varDef).ifPresent( json -> builder.add( WHEN_KEY, json));

    if( varDef.getValues() != null)
      {
      JsonObjectBuilder valuesBuilder = Json.createObjectBuilder();
      toStream( varDef.getValues()).forEach( value -> valuesBuilder.add( String.valueOf( value.getName()), toJson( value)));
      builder.add( VALUES_KEY, valuesBuilder.build());
      }
    else if( varDef.getMembers() != null)
      {
      JsonObjectBuilder membersBuilder = Json.createObjectBuilder();
      toStream( varDef.getMembers()).forEach( member -> membersBuilder.add( member.getName(), toJson( member)));
      builder.add( MEMBERS_KEY, membersBuilder.build());
      }
      
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given value definition.
   */
  private static JsonStructure toJson( VarValueDef value)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    if( value.getType().equals( FAILURE))
      {
      builder.add( FAILURE_KEY, true);
      }
    else if( value.getType().equals( ONCE))
      {
      builder.add( ONCE_KEY, true);
      }

    addAnnotations( builder, value);

    ConditionJson.toJson( value).ifPresent( json -> builder.add( WHEN_KEY, json));
      
    addProperties( builder, value);

    return builder.build();
    }

  /**
   * Add any annotatations from the given Annotated object to the given JsonObjectBuilder.
   */
  private static JsonObjectBuilder addAnnotations( JsonObjectBuilder builder, IAnnotated annotated)
    {
    JsonObjectBuilder annotations = Json.createObjectBuilder();
    toStream( annotated.getAnnotations()).forEach( name -> annotations.add( name, annotated.getAnnotation( name)));
    JsonObject json = annotations.build();

    if( !json.isEmpty())
      {
      builder.add( HAS_KEY, json);
      }

    return builder;
    }

  /**
   * Add any properties from the given value to the given JsonObjectBuilder.
   */
  private static JsonObjectBuilder addProperties( JsonObjectBuilder builder, VarValueDef value)
    {
    JsonArrayBuilder properties = Json.createArrayBuilder();
    value.getProperties().forEach( property -> properties.add( property));
    JsonArray json = properties.build();

    if( !json.isEmpty())
      {
      builder.add( PROPERTIES_KEY, json);
      }

    return builder;
    }

  /**
   * Returns the SystemInputDef represented by the given JSON object.
   */
  public static SystemInputDef asSystemInputDef( JsonObject json)
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
  private static FunctionInputDef asFunctionInputDef( String functionName, JsonObject json)
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
  private static Stream<IVarDef> getVarDefs( String varType, JsonObject json)
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
  private static IVarDef asVarDef( String varName, String varType, Annotated groupAnnotations, JsonObject json)
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
  private static Stream<VarValueDef> getValueDefs( JsonObject json)
    {
    return
      json.keySet().stream()
      .map( valueName -> asValueDef( valueName, json.getJsonObject( valueName)));
    }

  /**
   * Returns the value definition represented by the given JSON object.
   */
  private static VarValueDef asValueDef( String valueName, JsonObject json)
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
  private static ICondition asValidCondition( JsonObject json)
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
  private static ICondition asCondition( JsonObject json)
    {
    List<Function<JsonObject,Optional<ICondition>>> converters = Arrays.asList
      (
        SystemInputJson::asContainsAll,
        SystemInputJson::asContainsAny,
        SystemInputJson::asContainsNone,
        SystemInputJson::asNot,
        SystemInputJson::asAnyOf,
        SystemInputJson::asAllOf,
        SystemInputJson::asLessThan,
        SystemInputJson::asMoreThan,
        SystemInputJson::asNotLessThan,
        SystemInputJson::asNotMoreThan,
        SystemInputJson::asBetween,
        SystemInputJson::asEquals
       );

    return
      converters.stream()
      .map( converter -> converter.apply( json))
      .filter( Optional::isPresent)
      .map( Optional::get)
      .findFirst()
      .orElseThrow( () -> new SystemInputException( String.format( "Unknown condition type: %s", json.keySet().iterator().next())))
      ;
    }

  /**
   * Returns the ContainsAll condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asContainsAll( JsonObject json)
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
  private static Optional<ICondition> asContainsAny( JsonObject json)
    {
    return
      json.containsKey( HAS_ANY_KEY)
      ? Optional.of( new ContainsAny( toIdentifiers( json.getJsonArray( HAS_ANY_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the Not( ContainsAny) condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asContainsNone( JsonObject json)
    {
    return
      json.containsKey( HAS_NONE_KEY)
      ? Optional.of( new Not( new ContainsAny( toIdentifiers( json.getJsonArray( HAS_NONE_KEY)))))
      : Optional.empty();
    }

  /**
   * Returns the Not condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asNot( JsonObject json)
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
  private static Optional<ICondition> asAllOf( JsonObject json)
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
  private static Optional<ICondition> asAnyOf( JsonObject json)
    {
    return
      json.containsKey( ANY_OF_KEY)
      ? Optional.of( new AnyOf( toConditions( json.getJsonArray( ANY_OF_KEY))))
      : Optional.empty();
    }

  /**
   * Returns the AssertLess condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asLessThan( JsonObject json)
    {
    return
      Optional.of( json)
      .filter( j -> j.containsKey( LESS_THAN_KEY))
      .map( j -> j.getJsonObject( LESS_THAN_KEY))
      .map( a -> new AssertLess( a.getString( PROPERTY_KEY), a.getInt( MAX_KEY)));
    }

  /**
   * Returns the AssertMore condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asMoreThan( JsonObject json)
    {
    return
      Optional.of( json)
      .filter( j -> j.containsKey( MORE_THAN_KEY))
      .map( j -> j.getJsonObject( MORE_THAN_KEY))
      .map( a -> new AssertMore( a.getString( PROPERTY_KEY), a.getInt( MIN_KEY)));
    }

  /**
   * Returns the AssertNotLess condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asNotLessThan( JsonObject json)
    {
    return
      Optional.of( json)
      .filter( j -> j.containsKey( NOT_LESS_THAN_KEY))
      .map( j -> j.getJsonObject( NOT_LESS_THAN_KEY))
      .map( a -> new AssertNotLess( a.getString( PROPERTY_KEY), a.getInt( MIN_KEY)));
    }

  /**
   * Returns the AssertNotMore condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asNotMoreThan( JsonObject json)
    {
    return
      Optional.of( json)
      .filter( j -> j.containsKey( NOT_MORE_THAN_KEY))
      .map( j -> j.getJsonObject( NOT_MORE_THAN_KEY))
      .map( a -> new AssertNotMore( a.getString( PROPERTY_KEY), a.getInt( MAX_KEY)));
    }

  /**
   * Returns the between condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asBetween( JsonObject json)
    {
    return
      Optional.of( json)
      .filter( j -> j.containsKey( BETWEEN_KEY))
      .map( j -> j.getJsonObject( BETWEEN_KEY))
      .map( b -> 
        new Between(
          b.containsKey( EXCLUSIVE_MIN_KEY)
          ? new AssertMore( b.getString( PROPERTY_KEY), b.getInt( EXCLUSIVE_MIN_KEY))
          : new AssertNotLess( b.getString( PROPERTY_KEY), b.getInt( MIN_KEY)),

          b.containsKey( EXCLUSIVE_MAX_KEY)
          ? new AssertLess( b.getString( PROPERTY_KEY), b.getInt( EXCLUSIVE_MAX_KEY))
          : new AssertNotMore( b.getString( PROPERTY_KEY), b.getInt( MAX_KEY))));
    }

  /**
   * Returns the equals condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private static Optional<ICondition> asEquals( JsonObject json)
    {
    return
      Optional.of( json)
      .filter( j -> j.containsKey( EQUALS_KEY))
      .map( j -> j.getJsonObject( EQUALS_KEY))
      .map( e -> new Equals( e.getString( PROPERTY_KEY), e.getInt( COUNT_KEY)));
    }

  /**
   * Returns the contents of the given JSON array as a list of identifiers.
   */
  private static List<String> toIdentifiers( JsonArray array)
    {
    return
      IntStream.range( 0, array.size())
      .mapToObj( i -> validIdentifier( array.getString( i)))
      .collect( toList());
    }

  /**
   * Returns the contents of the given JSON array as an array of conditions.
   */
  private static ICondition[] toConditions( JsonArray array)
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
  private static String validIdentifier( String string)
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

  private static class ConditionJson implements IConditionVisitor
    {
    public static Optional<JsonObject> toJson( IConditional conditional)
      {
      return
        Optional.ofNullable( conditional.getCondition())
        .map( condition -> ConditionJson.toJson( condition));
      }

    private static JsonObject toJson( ICondition condition)
      {
      return new ConditionJson( condition).toJson();
      }
    
    private ConditionJson( ICondition condition)
      {
      condition_ = condition;
      }
    
    private JsonObject toJson()
      {
      condition_.accept( this);
      return json_;
      }
    
    public void visit( AllOf condition)
      {
      JsonArrayBuilder conditions = Json.createArrayBuilder();
      toStream( condition.getConditions()).forEach( c -> conditions.add( toJson( c)));

      json_ =
        Json.createObjectBuilder()
        .add( ALL_OF_KEY, conditions)
        .build();
      }
  
    public void visit( AnyOf condition)
      {
      JsonArrayBuilder conditions = Json.createArrayBuilder();
      toStream( condition.getConditions()).forEach( c -> conditions.add( toJson( c)));

      json_ =
        Json.createObjectBuilder()
        .add( ANY_OF_KEY, conditions)
        .build();
      }
  
    public void visit( ContainsAll condition)
      {
      JsonArrayBuilder properties = Json.createArrayBuilder();
      toStream( condition.getProperties()).forEach( property -> properties.add( property));

      json_ =
        Json.createObjectBuilder()
        .add( HAS_ALL_KEY, properties)
        .build();
      }
  
    public void visit( ContainsAny condition)
      {
      JsonArrayBuilder properties = Json.createArrayBuilder();
      toStream( condition.getProperties()).forEach( property -> properties.add( property));

      json_ =
        Json.createObjectBuilder()
        .add( HAS_ANY_KEY, properties)
        .build();
      }
  
    public void visit( IConjunct condition)
      {
      throw new UnsupportedOperationException( "Unexpected IConjunct in SystemInputDef");
      }
  
    public void visit( Not condition)
      {
      ICondition[] conditions = IteratorUtils.toArray( condition.getConditions(), ICondition.class);

      JsonObjectBuilder builder = Json.createObjectBuilder();
      if( conditions.length > 1)
        {
        builder.add( NOT_KEY, toJson( new AnyOf( conditions)));
        }
      else if( conditions[0].getClass().equals( ContainsAny.class))
        {
        // Special case: abbreviate "not:{hasAny:[...]}" as "hasNone:[...]".
        JsonArrayBuilder properties = Json.createArrayBuilder();
        toStream( ((ContainsAny) conditions[0]).getProperties()).forEach( property -> properties.add( property));
        builder.add( HAS_NONE_KEY, properties);
        }
      else
        {
        builder.add( NOT_KEY, toJson( conditions[0]));
        }
      
      json_ = builder.build();
      }

    public void visit( AssertLess condition)
      {
      json_ =
        Json.createObjectBuilder()
        .add(
          LESS_THAN_KEY,

          Json.createObjectBuilder()
          .add( PROPERTY_KEY, condition.getProperty())
          .add( MAX_KEY, condition.getBound()))
        
        .build();
      }

    public void visit( AssertMore condition)
      {
      json_ =
        Json.createObjectBuilder()
        .add(
          MORE_THAN_KEY,

          Json.createObjectBuilder()
          .add( PROPERTY_KEY, condition.getProperty())
          .add( MIN_KEY, condition.getBound()))
        
        .build();
      }

    public void visit( AssertNotLess condition)
      {
      json_ =
        Json.createObjectBuilder()
        .add(
          NOT_LESS_THAN_KEY,

          Json.createObjectBuilder()
          .add( PROPERTY_KEY, condition.getProperty())
          .add( MIN_KEY, condition.getBound()))
        
        .build();
      }

    public void visit( AssertNotMore condition)
      {
      json_ =
        Json.createObjectBuilder()
        .add(
          NOT_MORE_THAN_KEY,

          Json.createObjectBuilder()
          .add( PROPERTY_KEY, condition.getProperty())
          .add( MAX_KEY, condition.getBound()))
        
        .build();
      }

    public void visit( Between condition)
      {
      BoundedAssertion min = condition.getMin();
      BoundedAssertion max = condition.getMax();

      json_ =
        Json.createObjectBuilder()
        .add(
          BETWEEN_KEY,

          Json.createObjectBuilder()
          .add( PROPERTY_KEY, min.getProperty())
          .add( min.isExclusive()? EXCLUSIVE_MIN_KEY : MIN_KEY, min.getBound())
          .add( max.isExclusive()? EXCLUSIVE_MAX_KEY : MAX_KEY, max.getBound()))

        .build();
      }

    public void visit( Equals condition)
      {
      BoundedAssertion min = condition.getMin();

      json_ =
        Json.createObjectBuilder()
        .add(
          EQUALS_KEY,

          Json.createObjectBuilder()
          .add( PROPERTY_KEY, min.getProperty())
          .add( COUNT_KEY, min.getBound()))

        .build();
      }

    private ICondition condition_;
    private JsonObject json_;
    }

  private static final String ALL_OF_KEY = "allOf";
  private static final String ANY_OF_KEY = "anyOf";
  private static final String BETWEEN_KEY = "between";
  private static final String COUNT_KEY = "count";
  private static final String EQUALS_KEY = "equals";
  private static final String EXCLUSIVE_MAX_KEY = "exclusiveMax";
  private static final String EXCLUSIVE_MIN_KEY = "exclusiveMin";
  private static final String FAILURE_KEY = "failure";
  private static final String HAS_ALL_KEY = "hasAll";
  private static final String HAS_ANY_KEY = "hasAny";
  private static final String HAS_KEY = "has";
  private static final String HAS_NONE_KEY = "hasNone";
  private static final String LESS_THAN_KEY = "lessThan";
  private static final String MAX_KEY = "max";
  private static final String MEMBERS_KEY = "members";
  private static final String MIN_KEY = "min";
  private static final String MORE_THAN_KEY = "moreThan";
  private static final String NOT_KEY = "not";
  private static final String NOT_LESS_THAN_KEY = "notLessThan";
  private static final String NOT_MORE_THAN_KEY = "notMoreThan";
  private static final String ONCE_KEY = "once";
  private static final String PROPERTIES_KEY = "properties";
  private static final String PROPERTY_KEY = "property";
  private static final String SYSTEM_KEY = "system";
  private static final String VALUES_KEY = "values";
  private static final String WHEN_KEY = "when";
  }
