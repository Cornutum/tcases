//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.DefUtils;
import org.cornutum.tcases.conditions.*;
import org.cornutum.tcases.resolve.*;
import org.cornutum.tcases.resolve.DataValue.Type;
import org.cornutum.tcases.util.ContextHandler;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.ObjectUtils;
import static org.cornutum.tcases.resolve.DataValue.Type.*;
import static org.cornutum.tcases.resolve.DataValues.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.collections4.IteratorUtils;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;

/**
 * Converts between a {@link SystemInputDef} and its corresponding {@link JsonObject}.
 */
public class SystemInputJson extends ContextHandler<SystemInputContext>
  {
  public SystemInputJson( SystemInputContext context)
    {
    super( context);
    schemas_ = new Schemas( getContext());
    }

  /**
   * Returns the JSON object that represents the given system input definition.
   */
  public JsonObject toJson( SystemInputDef systemInput)
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
  private JsonStructure toJson( FunctionInputDef functionInput)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, functionInput);

    Arrays.stream( functionInput.getVarTypes()).forEach( varType -> builder.add( varType, toJson( functionInput, varType)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the function variables of the given type.
   */
  private JsonStructure toJson( FunctionInputDef functionInput, String varType)
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
  private JsonStructure toJson( IVarDef varDef)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, varDef);

    ConditionJson.toJson( varDef).ifPresent( json -> builder.add( WHEN_KEY, json));

    if( varDef.getValues() != null)
      {
      Optional.ofNullable( ((VarDef) varDef).getSchema())
        .ifPresent( schema -> addSchema( builder, schema));

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
  private JsonStructure toJson( VarValueDef value)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    if( value.getType().equals( VarValueDef.Type.FAILURE))
      {
      builder.add( FAILURE_KEY, true);
      }
    else if( value.getType().equals( VarValueDef.Type.ONCE))
      {
      builder.add( ONCE_KEY, true);
      }

    addAnnotations( builder, value);

    ConditionJson.toJson( value).ifPresent( json -> builder.add( WHEN_KEY, json));
      
    addProperties( builder, value);

    Optional.ofNullable( value.getSchema())
      .ifPresent( schema -> addSchema( builder, schema));
      
    return builder.build();
    }

  /**
   * Add the given schema definition to the given JsonObjectBuilder.
   */
  private JsonObjectBuilder addSchema( JsonObjectBuilder builder, Schema schema)
    {
    Optional.of( schema.getType())
      .filter( type -> type != NULL)
      .ifPresent( type -> builder.add( TYPE_KEY, type.toString().toLowerCase()));

    Optional.ofNullable( schema.getConstant()).ifPresent( constant -> builder.add( CONST_KEY, DataValueJson.toJson( constant)));
    Optional.ofNullable( schema.getFormat()).ifPresent( format -> builder.add( FORMAT_KEY, format));
    Optional.ofNullable( schema.getMinimum()).ifPresent( minimum -> builder.add( MINIMUM_KEY, minimum));
    Optional.ofNullable( schema.getMaximum()).ifPresent( maximum -> builder.add( MAXIMUM_KEY, maximum));
    Optional.ofNullable( schema.getExclusiveMinimum()).ifPresent( exclusiveMinimum -> builder.add( EXCLUSIVE_MINIMUM_KEY, exclusiveMinimum));
    Optional.ofNullable( schema.getExclusiveMaximum()).ifPresent( exclusiveMaximum -> builder.add( EXCLUSIVE_MAXIMUM_KEY, exclusiveMaximum));
    Optional.ofNullable( schema.getMultipleOf()).ifPresent( multipleOf -> builder.add( MULTIPLE_OF_KEY, multipleOf));
    Optional.ofNullable( schema.getMinLength()).ifPresent( minLength -> builder.add( MIN_LENGTH_KEY, minLength));
    Optional.ofNullable( schema.getMaxLength()).ifPresent( maxLength -> builder.add( MAX_LENGTH_KEY, maxLength));
    Optional.ofNullable( schema.getPattern()).ifPresent( pattern -> builder.add( PATTERN_KEY, pattern));
    Optional.ofNullable( schema.getMinItems()).ifPresent( minItems -> builder.add( MIN_ITEMS_KEY, minItems));
    Optional.ofNullable( schema.getMaxItems()).ifPresent( maxItems -> builder.add( MAX_ITEMS_KEY, maxItems));
    Optional.ofNullable( schema.getUniqueItems()).ifPresent( uniqueItems -> builder.add( UNIQUE_ITEMS_KEY, uniqueItems));
    Optional.ofNullable( schema.getItems()).ifPresent( items -> builder.add( ITEMS_KEY, toJson( items)));

    return builder;
    }

  /**
   * Returns the JSON object that represents the given schema.
   */
  private JsonObject toJson( Schema schema)
    {
    return addSchema( Json.createObjectBuilder(), schema).build();
    }

  /**
   * Add any annotatations from the given Annotated object to the given JsonObjectBuilder.
   */
  private JsonObjectBuilder addAnnotations( JsonObjectBuilder builder, IAnnotated annotated)
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
  private JsonObjectBuilder addProperties( JsonObjectBuilder builder, VarValueDef value)
    {
    JsonArrayBuilder properties = Json.createArrayBuilder();
    propertySeq( value.getProperties()).forEach( property -> properties.add( property));
    JsonArray json = properties.build();

    if( !json.isEmpty())
      {
      builder.add( PROPERTIES_KEY, json);
      }

    return builder;
    }

  /**
   * Returns an ordered sequence of property names.
   */
  private static Stream<String> propertySeq( Iterable<String> properties)
    {
    return propertySeq( properties.iterator());
    }

  /**
   * Returns an ordered sequence of property names.
   */
  private static Stream<String> propertySeq( Iterator<String> properties)
    {
    return toStream( properties).sorted();
    }

  /**
   * Returns the SystemInputDef represented by the given JSON object.
   */
  public SystemInputDef asSystemInputDef( JsonObject json)
    {
    String systemName =
      Optional.ofNullable( json.getString( SYSTEM_KEY, null))
      .orElseThrow( () -> new SystemInputException( "No system name defined"));

    return
      resultFor( systemName, () -> {
        SystemInputDef systemInputDef = new SystemInputDef( validIdentifier( systemName));
      
        // Get system annotations
        Optional.ofNullable( json.getJsonObject( HAS_KEY))
          .ifPresent( has -> has.keySet().stream().forEach( key -> systemInputDef.setAnnotation( key, has.getString( key))));

        // Get system functions
        json.keySet().stream()
          .filter( key -> !(key.equals( SYSTEM_KEY) || key.equals( HAS_KEY)))
          .forEach( function -> systemInputDef.addFunctionInputDef( asFunctionInputDef( function, json.getJsonObject( function))));

        return systemInputDef;
        });
    }

  /**
   * Returns the FunctionInputDef represented by the given JSON object.
   */
  private FunctionInputDef asFunctionInputDef( String functionName, JsonObject json)
    {
    return
      resultFor( functionName, () -> {
        FunctionInputDef functionInputDef = new FunctionInputDef( validIdentifier( functionName));
      
        // Get function annotations
        Optional.ofNullable( json.getJsonObject( HAS_KEY))
          .ifPresent( has -> has.keySet().stream().forEach( key -> functionInputDef.setAnnotation( key, has.getString( key))));

        // Get function variables.
        json.keySet().stream()
          .filter( key -> !key.equals( HAS_KEY))
          .forEach( varType -> {
            resultFor( varType, () -> getVarDefs( varType, json.getJsonObject( varType)))
              .forEach( varDef -> functionInputDef.addVarDef( varDef));
            });

        if( functionInputDef.getVarTypes().length == 0)
          {
          throw new IllegalStateException( String.format( "No variables defined for function=%s", functionName));
          }

        SystemInputs systemInputs = new SystemInputs( getContext().getPrevLocation());
        systemInputs.getPropertiesUnused( functionInputDef)
          .forEach( (property,valueDefs) -> {
            valueDefs.stream()
              .forEach(
                valueDef ->
                notifyWarning(
                  valueDef.getLocation(),
                  String.format( "Property=%s is unused", property)));
            });
          
        systemInputs.getPropertiesUndefined( functionInputDef)
          .entrySet().stream()
          .findFirst()
          .ifPresent( undefined -> {
            String property = undefined.getKey();
            String[] refLocation = undefined.getValue().iterator().next().getLocation();
            throw new SystemInputException( refLocation, String.format( "Depends on undefined property=%s", property));
            });

        return functionInputDef;
        });
    }

  /**
   * Returns the variable definitions represented by the given JSON object.
   */
  private Stream<IVarDef> getVarDefs( String varType, JsonObject json)
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

  /**
   * Returns the variable definition represented by the given JSON object.
   */
  private IVarDef asVarDef( String varName, String varType, Annotated groupAnnotations, JsonObject json)
    {
    return 
      resultFor( varName, () -> {
        validIdentifier( varName);

        AbstractVarDef  varDef =
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

          if( getSchema( json) != null)
            {
            throw new IllegalStateException( "A schema cannot be defined for a variable set");
            }
        
          getVarDefs( varType, json.getJsonObject( MEMBERS_KEY))
            .forEach( member -> varSet.addMember( member));

          if( !varSet.getMembers().hasNext())
            {
            throw new IllegalStateException( String.format( "No members defined for variable=%s", varName));
            }
          }
        else
          {
          VarDef var = (VarDef) varDef;

          // Get the schema for this variable
          var.setSchema( getSchema( json));

          Optional<Type> defaultNumericType =
            Optional.ofNullable( var.getSchema())
            .map( Schema::getType)
            .filter( Type::isNumeric);

          Optional.ofNullable( json.getJsonObject( VALUES_KEY))
            .ifPresent( values -> getValueDefs( defaultNumericType, values).forEach( valueDef -> var.addValue( valueDef)));

          if( var.getValues().hasNext() || var.getSchema() == null)
            {
            if( !var.getValidValues().hasNext())
              {
              throw new IllegalStateException( String.format( "No valid values defined for variable=%s", varName));
              }
            }
          }

        // Add any group annotations
        varDef.addAnnotations( groupAnnotations); 

        return varDef;
        });
    }

  /**
   * Returns the value definitions represented by the given JSON object.
   */
  private Stream<VarValueDef> getValueDefs( Optional<Type> defaultNumericType, JsonObject json)
    {
    return
      json.keySet().stream()
      .map( valueName -> asValueDef( defaultNumericType, valueName, json.getJsonObject( valueName)));
    }

  /**
   * Returns the value definition represented by the given JSON object.
   */
  private VarValueDef asValueDef( Optional<Type> defaultNumericType, String valueName, JsonObject json)
    {
    return
      resultFor( valueName, () -> {
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
          throw new IllegalStateException( "Failure type values can't define properties");
          }

        // Get annotations for this value
        Optional.ofNullable( json.getJsonObject( HAS_KEY))
          .ifPresent( has -> has.keySet().stream().forEach( key -> valueDef.setAnnotation( key, has.getString( key))));
    
        // Get the condition for this value
        Optional.ofNullable( json.getJsonObject( WHEN_KEY))
          .ifPresent( object -> valueDef.setCondition( asValidCondition( object)));

        // Get properties for this value
        doFor( PROPERTIES_KEY, () -> {
          Optional.ofNullable( json.getJsonArray( PROPERTIES_KEY))
            .map( properties -> toIdentifiers( properties)) 
            .ifPresent( properties -> valueDef.addProperties( properties));
          });

        // Get the schema for this variable
        valueDef.setSchema( getSchema( defaultNumericType, json)); 

        return valueDef;
        });
    }

  /**
   * Returns the condition definition represented by the given JSON object.
   */
  private ICondition asValidCondition( JsonObject json)
    {
    return resultFor( WHEN_KEY, () -> asCondition( json));
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
        this::asContainsNone,
        this::asNot,
        this::asAnyOf,
        this::asAllOf,
        this::asLessThan,
        this::asMoreThan,
        this::asNotLessThan,
        this::asNotMoreThan,
        this::asBetween,
        this::asEquals
       );

    return
      converters.stream()
      .map( converter -> converter.apply( json))
      .filter( Optional::isPresent)
      .map( Optional::get)
      .findFirst()
      .orElseThrow( () -> new IllegalStateException( String.format( "Unknown condition type: %s", json.keySet().iterator().next())))
      ;
    }

  /**
   * Returns the ContainsAll condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asContainsAll( JsonObject json)
    {
    return
      resultFor(
        HAS_ALL_KEY,
        () ->
        json.containsKey( HAS_ALL_KEY)
        ? Optional.of( new ContainsAll( toIdentifiers( json.getJsonArray( HAS_ALL_KEY))))
        : Optional.empty());
    }

  /**
   * Returns the ContainsAny condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asContainsAny( JsonObject json)
    {
    return
      resultFor(
        HAS_ANY_KEY,
        () ->
        json.containsKey( HAS_ANY_KEY)
        ? Optional.of( new ContainsAny( toIdentifiers( json.getJsonArray( HAS_ANY_KEY))))
        : Optional.empty());
    }

  /**
   * Returns the Not( ContainsAny) condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asContainsNone( JsonObject json)
    {
    return
      resultFor(
        HAS_NONE_KEY,
        () ->
        json.containsKey( HAS_NONE_KEY)
        ? Optional.of( new Not( new ContainsAny( toIdentifiers( json.getJsonArray( HAS_NONE_KEY)))))
        : Optional.empty());
    }

  /**
   * Returns the Not condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asNot( JsonObject json)
    {
    return
      resultFor(
        NOT_KEY,
        () ->
        json.containsKey( NOT_KEY)
        ? Optional.of( new Not( asCondition( json.getJsonObject( NOT_KEY))))
        : Optional.empty());
    }

  /**
   * Returns the AllOf condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asAllOf( JsonObject json)
    {
    return
      resultFor(
        ALL_OF_KEY,
        () ->
        json.containsKey( ALL_OF_KEY)
        ? Optional.of( new AllOf( toConditions( json.getJsonArray( ALL_OF_KEY))))
        : Optional.empty());
    }

  /**
   * Returns the AnyOf condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asAnyOf( JsonObject json)
    {
    return
      resultFor(
        ANY_OF_KEY,
        () ->
        json.containsKey( ANY_OF_KEY)
        ? Optional.of( new AnyOf( toConditions( json.getJsonArray( ANY_OF_KEY))))
        : Optional.empty());
    }

  /**
   * Returns the AssertLess condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asLessThan( JsonObject json)
    {
    return
      resultFor(
        LESS_THAN_KEY,
        () ->
        Optional.of( json)
        .filter( j -> j.containsKey( LESS_THAN_KEY))
        .map( j -> j.getJsonObject( LESS_THAN_KEY))
        .map( a -> new AssertLess( a.getString( PROPERTY_KEY), a.getInt( MAX_KEY))));
    }

  /**
   * Returns the AssertMore condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asMoreThan( JsonObject json)
    {
    return
      resultFor(
        MORE_THAN_KEY,
        () ->
        Optional.of( json)
        .filter( j -> j.containsKey( MORE_THAN_KEY))
        .map( j -> j.getJsonObject( MORE_THAN_KEY))
        .map( a -> new AssertMore( a.getString( PROPERTY_KEY), a.getInt( MIN_KEY))));
    }

  /**
   * Returns the AssertNotLess condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asNotLessThan( JsonObject json)
    {
    return
      resultFor(
        NOT_LESS_THAN_KEY,
        () ->
        Optional.of( json)
        .filter( j -> j.containsKey( NOT_LESS_THAN_KEY))
        .map( j -> j.getJsonObject( NOT_LESS_THAN_KEY))
        .map( a -> new AssertNotLess( a.getString( PROPERTY_KEY), a.getInt( MIN_KEY))));
    }

  /**
   * Returns the AssertNotMore condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asNotMoreThan( JsonObject json)
    {
    return
      resultFor(
        NOT_MORE_THAN_KEY,
        () ->
        Optional.of( json)
        .filter( j -> j.containsKey( NOT_MORE_THAN_KEY))
        .map( j -> j.getJsonObject( NOT_MORE_THAN_KEY))
        .map( a -> new AssertNotMore( a.getString( PROPERTY_KEY), a.getInt( MAX_KEY))));
    }

  /**
   * Returns the between condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asBetween( JsonObject json)
    {
    return
      resultFor(
        BETWEEN_KEY,
        () ->
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
                : new AssertNotMore( b.getString( PROPERTY_KEY), b.getInt( MAX_KEY)))));
    }

  /**
   * Returns the equals condition represented by the given JSON object or an empty
   * value if no such condition is found.
   */
  private Optional<ICondition> asEquals( JsonObject json)
    {
    return
      resultFor(
        EQUALS_KEY,
        () ->
        Optional.of( json)
        .filter( j -> j.containsKey( EQUALS_KEY))
        .map( j -> j.getJsonObject( EQUALS_KEY))
        .map( e -> new Equals( e.getString( PROPERTY_KEY), e.getInt( COUNT_KEY))));
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
   * Returns the schema represented by the given JSON object.
   */
  private Schema getSchema( JsonObject json)
    {
    return getSchema( Optional.empty(), json);
    }

  /**
   * Returns the schema represented by the given JSON object.
   */
  private Schema getSchema( Optional<Type> defaultNumericType, JsonObject json)
    {
    return
      getSchemaType( defaultNumericType, json)
      .map( type -> getSchema( type, json))
      .orElse( null);
    }

  /**
   * Returns the schema represented by the given JSON object.
   */
  private Schema getSchema( Type type, JsonObject json)
    {
    Schema schema = new Schema( type);

    if( json.containsKey( FORMAT_KEY))
      {
      schema.setFormat( json.getString( FORMAT_KEY));
      }

    switch( type)
      {
      case ARRAY:
        {
        if( json.containsKey( CONST_KEY))
          {
          schema.setConstant( asDataValue( json, CONST_KEY, type, schema.getFormat()));
          }
        else
          {
          schema.setMinItems( asInteger( json, MIN_ITEMS_KEY));
          schema.setMaxItems( asInteger( json, MAX_ITEMS_KEY));
          schema.setUniqueItems( asBoolean( json, UNIQUE_ITEMS_KEY));
          schema.setItems( asSchema( json, ITEMS_KEY));
          }
        break;
        }

      case BOOLEAN:
        {
        if( json.containsKey( CONST_KEY))
          {
          schema.setConstant( asDataValue( json, CONST_KEY, type, schema.getFormat()));
          }
        break;
        }

      case INTEGER:
      case NUMBER:
        {
        if( json.containsKey( CONST_KEY))
          {
          schema.setConstant( asDataValue( json, CONST_KEY, type, schema.getFormat()));
          }
        else
          {
          schema.setMinimum( asBigDecimal( json, MINIMUM_KEY, type, schema.getFormat()));
          schema.setMaximum( asBigDecimal( json, MAXIMUM_KEY, type, schema.getFormat()));
          schema.setExclusiveMinimum( asBigDecimal( json, EXCLUSIVE_MINIMUM_KEY, type, schema.getFormat()));
          schema.setExclusiveMaximum( asBigDecimal( json, EXCLUSIVE_MAXIMUM_KEY, type, schema.getFormat()));
          schema.setMultipleOf( asBigDecimal( json, MULTIPLE_OF_KEY, type, schema.getFormat()));
          }
        break;
        }

      case STRING:
        {
        if( json.containsKey( CONST_KEY))
          {
          schema.setConstant( asDataValue( json, CONST_KEY, type, schema.getFormat()));
          }
        else
          {
          if( json.containsKey( MIN_LENGTH_KEY))
            {
            schema.setMinLength( json.getInt( MIN_LENGTH_KEY));
            }
          if( json.containsKey( MAX_LENGTH_KEY))
            {
            schema.setMaxLength( json.getInt( MAX_LENGTH_KEY));
            }
          if( json.containsKey( PATTERN_KEY))
            {
            schema.setPattern( json.getString( PATTERN_KEY));
            }
          }
        break;
        }

      case NULL:
        {
        if( json.containsKey( CONST_KEY))
          {
          schema.setConstant( asDataValue( json, CONST_KEY, type, schema.getFormat()));
          }
        break;
        }

      default:
        {
        break;
        }
      }
    
    return schemas_.normalize( schema);
    }

  /**
   * Returns the type of the schema represented by the given JSON object.
   */
  private Optional<Type> getSchemaType( Optional<Type> defaultNumericType, JsonObject json)
    {
    Type declaredType =
      Optional.ofNullable( json.getString( TYPE_KEY, null))
      .map( type -> Type.valueOf( type.toUpperCase()))
      .orElse( null);

    Map<Type,String> impliedTypes =
      schemaTypeKeys_.keySet().stream()
      .filter( type -> type != NULL)
      .collect(
        HashMap::new,

        (implied,type) -> {
          Optional.ofNullable( schemaTypeKeys_.get( type))
          .flatMap( keys -> keys.stream().filter( key -> json.containsKey( key)).findFirst())
          .ifPresent( key -> implied.put( type, key));
        },

        Map::putAll);

    // Do schema keywords imply a specific type?
    Type schemaType;
    if( impliedTypes.size() == 1)
      {
      // Yes, reconcile implied type with declared type.
      Type impliedType = impliedTypes.keySet().iterator().next();
      if( declaredType == null)
        {
        schemaType = defaultNumericType.filter( type -> impliedType.isNumeric()).orElse( impliedType);
        }
      else if( impliedType == declaredType || (impliedType.isNumeric() && declaredType == INTEGER))
        {
        schemaType = declaredType;
        }
      else
        {
        throw
          new IllegalStateException(
            String.format(
              "Schema declares type=%s but defines property=%s which implies type=%s",
              String.valueOf( declaredType).toLowerCase(),
              impliedTypes.get( impliedType),
              String.valueOf( impliedType).toLowerCase()));
        }
      }

    // Do schema keywords imply multiple types?
    else if( impliedTypes.size() > 1)
      {
      // Yes, not allowed
      throw
        new IllegalStateException(
          String.format(
            "Ambiguous schema type -- defines properties=%s which implies types=%s",
            impliedTypes.keySet().stream().map( type -> impliedTypes.get( type)).collect( joining( ", ", "[", "]")),
            impliedTypes.keySet().stream().map( type -> String.valueOf( type).toLowerCase()).collect( joining( ", ", "[", "]"))));
      }

    else
      {
      // No type-specific keywords defined.
      schemaType = declaredType;
      }

    // Does the schema define a "format" value?
    Type formatType = getFormatType( json);
    if( formatType != null)
      {
      // Yes, reconcile format type with schema type.
      if( schemaType == null)
        {
        schemaType = formatType;
        }
      else if( declaredType == null && schemaType.isNumeric() && formatType == INTEGER)
        {
        schemaType = formatType;
        }
      else if( formatType != schemaType)
        {
        throw
          new IllegalStateException(
            String.format(
              "Schema has type=%s but defines a '%s' of type=%s",
              String.valueOf( schemaType).toLowerCase(),
              FORMAT_KEY,
              String.valueOf( formatType).toLowerCase()));
        }
      }

    // Does the schema define a "const" value?
    Type constType = getConstType( json);
    if( constType != null)
      {
      // Yes, reconcile const type with schema type.
      if( schemaType == null)
        {
        schemaType = constType;
        }
      else if( !( constType == schemaType || constType == NULL || (constType == INTEGER && schemaType == NUMBER)))
        {
        throw
          new IllegalStateException(
            String.format(
              "Schema has type=%s but defines a '%s' of type=%s",
              String.valueOf( schemaType).toLowerCase(),
              CONST_KEY,
              String.valueOf( constType).toLowerCase()));
        }
      }
    
    return Optional.ofNullable( schemaType);
    }

  /**
   * If the given JSON object defines a format that implied a specific data type, returns the type.
   * Otherwise, returns null.
   */
  private Type getFormatType( JsonObject json)
    {
    return
      Optional.ofNullable( json.getString( FORMAT_KEY, null))
      .flatMap( format -> schemaTypeFormats_.keySet().stream().filter( type -> schemaTypeFormats_.get( type).contains( format)).findFirst())
      .orElse( null);
    }

  /**
   * If the given JSON object defines a value for the "const" property, returns the type of the value.
   * Otherwise, returns null.
   */
  private Type getConstType( JsonObject json)
    {
    return
      Optional.ofNullable( asDataValue( json, CONST_KEY, NULL, json.getString( FORMAT_KEY, null)))
      .map( DataValue::getType)
      .orElse( null);
    }

  /**
   * If the given JSON object defines a value for the given key, returns the value.
   * Otherwise, returns null.
   */
  private DataValue<?> asDataValue( JsonObject json, String key, Type expectedType, String format)
    {
    return
      resultFor( key, () -> {
        return
          Optional.ofNullable( json.get( key))
          .map( value -> {
            DataValue<?> dataValue = asDataValue( value, format);
        
            if( !(expectedType == NULL
                  ||
                  expectedType == dataValue.getType()
                  ||
                  dataValue.getType() == NULL
                  ||
                  (expectedType == NUMBER && dataValue.getType() == INTEGER)))
              {
              throw
                new IllegalStateException(
                  String.format(
                    "Expected a value of type=%s, but found '%s'",
                    String.valueOf( expectedType).toLowerCase(),
                    value));
              }

            return dataValue;
            })
          .orElse( null);
        });
    }

  /**
   * Returns the DataValue represented by the given JSON value.
   */
  private DataValue<?> asDataValue( JsonValue json, String format)
    {
    DataValue<?> value;
    switch( json.getValueType())
      {
      case ARRAY:
        {
        value = arrayOfAny( ((JsonArray)json).stream().map( item -> asDataValue( item, null)).collect( toList()));
        break;
        }
      case STRING:
        {
        value = stringOf( format, ((JsonString) json).getString());
        break;
        }
      case NUMBER:
        {
        JsonNumber number = (JsonNumber) json;
        IntegerValue integerValue;
        LongValue longValue;
        
        value =
          !number.isIntegral()?
          valueOf( number.bigDecimalValue()) :

          (integerValue = asIntegerValue( number, format)) != null?
          integerValue :

          (longValue = asLongValue( number, format)) != null?
          longValue :

          valueOf( number.bigDecimalValue());
            
        break;
        }
      case TRUE:    
        {
        value = valueOf( true);
        break;
        }
      case FALSE:
        {
        value = valueOf( false);
        break;
        }
      case NULL:
        {
        value = nullValue();
        break;
        }
      default:
        {
        value = null;
        break;
        }
      }

    return value;
    }

  /**
   * Returns the IntegerValue represented by the given JSON value.
   * Returns null if this value is not an Integer.
   */
  private IntegerValue asIntegerValue( JsonNumber number, String format)
    {
    try
      {
      return
        "int64".equals( format)
        ? null
        : valueOf( number.intValueExact());
      }
    catch( Exception e)
      {
      if( "int32".equals( format))
        {
        throw new IllegalArgumentException( String.format( "Expected an int32 value but found %s", number));
        }
      return null;
      }
    }

  /**
   * Returns the LongValue represented by the given JSON value.
   * Returns null if this value is not a Long.
   */
  private LongValue asLongValue( JsonNumber number, String format)
    {
    try
      {
      return
        "int32".equals( format)
        ? null
        : valueOf( number.longValueExact());
      }
    catch( Exception e)
      {
      if( "int64".equals( format))
        {
        throw new IllegalArgumentException( String.format( "Expected an int64 value but found %s", number));
        }
      return null;
      }
    }

  /**
   * If the given JSON object defines a value for the given key, returns the value as an Integer.
   * Otherwise, returns null.
   */
  private Integer asInteger( JsonObject json, String key)
    {
    return
      Optional.ofNullable( asDataValue( json, key, INTEGER, "int32"))
      .map( value -> ((IntegerValue) value).getValue())
      .orElse( null);
    }

  /**
   * If the given JSON object defines a value for the given key, returns the value as a Boolean.
   * Otherwise, returns null.
   */
  private Boolean asBoolean( JsonObject json, String key)
    {
    return
      Optional.ofNullable( asDataValue( json, key, BOOLEAN, null))
      .map( value -> ((BooleanValue) value).getValue())
      .orElse( null);
    }

  /**
   * If the given JSON object defines a value for the given key, returns the value as a Schema.
   * Otherwise, returns null.
   */
  private Schema asSchema( JsonObject json, String key)
    {
    return
      resultFor( key, () -> {
        JsonObject schemaJson = 
          json.containsKey( key)
          ? json.getJsonObject( key)
          : null;

        return
          Optional.ofNullable( schemaJson)
          .map( schemaDef -> {

            schemaDef.keySet().stream()
              .filter( k -> schemaTypeKeys_.values().stream().noneMatch( typeKeys -> typeKeys.contains( k)))
              .findFirst()
              .ifPresent( k -> {
                throw new IllegalStateException( String.format( "Unknown schema key '%s'", k));
                });
          
            return
              Optional.ofNullable( getSchema( schemaDef))
              .orElseThrow( () -> new IllegalStateException( "Incomplete schema definition")); 
            })
          .orElse( null);
        });
    }

  /**
   * If the given JSON object defines a value for the given key, returns the value as a BigDecimal.
   * Otherwise, returns null.
   */
  private BigDecimal asBigDecimal( JsonObject json, String key, Type expectedType, String format)
    {
    return resultFor( key, () -> bigDecimalOf( asDataValue( json, key, expectedType, format)));
    }

  /**
   * Reports a SystemInputException if the given string is not a valid identifier. Otherwise, returns this string.
   */
  private String validIdentifier( String string)
    {
    DefUtils.assertIdentifier( string);
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
    
    @Override
    public void visit( AllOf condition)
      {
      JsonArrayBuilder conditions = Json.createArrayBuilder();
      toStream( condition.getConditions()).forEach( c -> conditions.add( toJson( c)));

      json_ =
        Json.createObjectBuilder()
        .add( ALL_OF_KEY, conditions)
        .build();
      }
  
    @Override
    public void visit( AnyOf condition)
      {
      JsonArrayBuilder conditions = Json.createArrayBuilder();
      toStream( condition.getConditions()).forEach( c -> conditions.add( toJson( c)));

      json_ =
        Json.createObjectBuilder()
        .add( ANY_OF_KEY, conditions)
        .build();
      }
  
    @Override
    public void visit( ContainsAll condition)
      {
      JsonArrayBuilder properties = Json.createArrayBuilder();
      propertySeq( condition.getProperties()).forEach( property -> properties.add( property));

      json_ =
        Json.createObjectBuilder()
        .add( HAS_ALL_KEY, properties)
        .build();
      }
  
    @Override
    public void visit( ContainsAny condition)
      {
      JsonArrayBuilder properties = Json.createArrayBuilder();
      propertySeq( condition.getProperties()).forEach( property -> properties.add( property));

      json_ =
        Json.createObjectBuilder()
        .add( HAS_ANY_KEY, properties)
        .build();
      }
  
    @Override
    public void visit( IConjunct condition)
      {
      throw new UnsupportedOperationException( "Unexpected IConjunct in SystemInputDef");
      }
  
    @Override
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
        propertySeq( ((ContainsAny) conditions[0]).getProperties()).forEach( property -> properties.add( property));
        builder.add( HAS_NONE_KEY, properties);
        }
      else
        {
        builder.add( NOT_KEY, toJson( conditions[0]));
        }
      
      json_ = builder.build();
      }

    @Override
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

    @Override
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

    @Override
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

    @Override
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

    @Override
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

    @Override
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

  private static class DataValueJson implements DataValueVisitor
    {
    private static JsonValue toJson( DataValue<?> data)
      {
      return new DataValueJson( data).toJson();
      }
    
    private DataValueJson( DataValue<?> data)
      {
      data_ = data;
      }
    
    private JsonValue toJson()
      {
      data_.accept( this);
      return json_;
      }

    @Override
	public void visit( ArrayValue<?> data)
      {
      JsonArrayBuilder items = Json.createArrayBuilder();
      data.getValue().forEach( item -> items.add( toJson( item)));
      json_ = items.build();
      }

    @Override
	public void visit( BinaryValue data)
      {
      throw new UnsupportedOperationException( "Binary data not supported");
      }

    @Override
	public void visit( BooleanValue data)
      {
      json_ = data.getValue()? JsonValue.TRUE : JsonValue.FALSE;
      }

    @Override
	public void visit( DecimalValue data)
      {
      json_ = Json.createValue( data.getValue());
      }

    @Override
	public void visit( IntegerValue data)
      {
      json_ = Json.createValue( data.getValue());
      }

    @Override
	public void visit( LongValue data)
      {
      json_ = Json.createValue( data.getValue());
      }

    @Override
	public void visit( NullValue data)
      {
      json_ = JsonValue.NULL;
      }

    @Override
	public void visit( ObjectValue data)
      {
      throw new UnsupportedOperationException( "type=object is not supported");
      }

    @Override
	public void visit( StringValue data)
      {
      json_ = Json.createValue( data.getValue());
      }
  
    private DataValue<?> data_;
    private JsonValue json_;
    }

  private final Schemas schemas_;
  
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

  private static final String CONST_KEY = "const";
  private static final String EXCLUSIVE_MAXIMUM_KEY = "exclusiveMaximum";
  private static final String EXCLUSIVE_MINIMUM_KEY = "exclusiveMinimum";
  private static final String FORMAT_KEY = "format";
  private static final String ITEMS_KEY = "items";
  private static final String MAXIMUM_KEY = "maximum";
  private static final String MAX_ITEMS_KEY = "maxItems";
  private static final String MAX_LENGTH_KEY = "maxLength";
  private static final String MINIMUM_KEY = "minimum";
  private static final String MIN_ITEMS_KEY = "minItems";
  private static final String MIN_LENGTH_KEY = "minLength";
  private static final String MULTIPLE_OF_KEY = "multipleOf";
  private static final String PATTERN_KEY = "pattern";
  private static final String TYPE_KEY = "type";
  private static final String UNIQUE_ITEMS_KEY = "uniqueItems";

  private static final Map<Type,List<String>> schemaTypeKeys_ =
    MapBuilder
    .of( NULL, Arrays.asList( TYPE_KEY, CONST_KEY, FORMAT_KEY))
    .put( ARRAY, Arrays.asList( ITEMS_KEY, MAX_ITEMS_KEY, MIN_ITEMS_KEY, UNIQUE_ITEMS_KEY))
    .put( NUMBER, Arrays.asList( MAXIMUM_KEY, EXCLUSIVE_MAXIMUM_KEY, MINIMUM_KEY, EXCLUSIVE_MINIMUM_KEY, MULTIPLE_OF_KEY))
    .put( STRING, Arrays.asList( MAX_LENGTH_KEY, MIN_LENGTH_KEY, PATTERN_KEY))
    .build();

  private static final Map<Type,List<String>> schemaTypeFormats_ =
    MapBuilder
    .of( INTEGER, Arrays.asList( "int32", "int64"))
    .put( STRING, Arrays.asList( "date-time", "date", "email", "uuid"))
    .build();
}
