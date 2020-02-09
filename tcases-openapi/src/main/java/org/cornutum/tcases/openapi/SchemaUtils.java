//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.openapi.SchemaExtensions.*;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

/**
 * Defines methods for accessing an OpenAPI {@link Schema} object.
 */
public final class SchemaUtils
  {
  /**
   * Creates a new SchemaUtils instance.
   */
  private SchemaUtils()
    {
    // Static methods only
    }

  /**
   * If necessary, updates the type of the given schema based its properties.
   * If the type defined for this schema is valid and consistent, returns the updated schema.
   * Otherwise, throws an exception.
   */
  public static Schema<?> resolveSchemaType( Schema<?> schema)
    {
    String declaredType = schema.getType();
    
    List<String> impliedTypes = 
    Stream.of(
      impliedType( "array", schema, Schema::getMaxItems, Schema::getMinItems, Schema::getUniqueItems),
      impliedType( "number", schema, Schema::getMaximum, Schema::getExclusiveMaximum, Schema::getMinimum, Schema::getExclusiveMinimum, Schema::getMultipleOf),
      impliedType( "object", schema, Schema::getMaxProperties, Schema::getMinProperties, Schema::getRequired, Schema::getProperties, Schema::getAdditionalProperties),
      impliedType( "string", schema, Schema::getMaxLength, Schema::getMinLength, Schema::getPattern))
      .filter( Objects::nonNull)
      .collect( toList());

    if( impliedTypes.size() == 1)
      {
      String impliedType = impliedTypes.get(0);
      if( declaredType == null)
        {
        schema.setType( impliedType);
        }
      else if( !(impliedType.equals( declaredType) || (impliedType.equals( "number") && declaredType.equals( "integer"))))
        {
        throw new IllegalStateException( String.format( "Schema declares type=%s but has implied type=%s", declaredType, impliedType));
        }
      }
    else if( impliedTypes.size() > 1)
      {
      throw new IllegalStateException( String.format( "Ambiguous schema type -- could be any of %s", impliedTypes.stream().collect( joining( ", "))));
      }
      
    return schema;
    }

  /**
   * Returns the given type if applying any of the given accessors to the given schema produces a non-null value.
   * Otherwise, returns null.
   */
  @SafeVarargs
  private static String impliedType( String type, Schema<?> schema, Function<Schema<?>,Object>... accessors)
    {
    return
      Stream.of( accessors)
      .map( accessor -> accessor.apply( schema))
      .filter( Objects::nonNull)
      .findFirst()
      .map( value -> type)
      .orElse( null);
    }

  /**
   * Returns true if the given schema type is supported by OpenAPI.
   */
  public static boolean isSchemaType( String type)
    {
    return type == null || SCHEMA_TYPES.contains( type);
    }

  /**
   * If the given schema is a ComposedSchema instance, returns the casting result.
   * Otherwise, returns null.
   */
  public static ComposedSchema asComposedSchema( Schema<?> schema)
    {
    return
      schema instanceof ComposedSchema
      ? (ComposedSchema) schema
      : null;
    }

  /**
   * If the given schema is an ArraySchema instance, returns the casting result.
   * Otherwise, returns null.
   */
  public static ArraySchema asArraySchema( Schema<?> schema)
    {
    return
      schema instanceof ArraySchema
      ? (ArraySchema) schema
      : null;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema<?> combined;

    if( base == null)
      {
      combined = additional;
      }
    else if( additional == null)
      {
      combined = base;
      }
    else
      {
      // A null type indicates an empty schema, which can be combined with any type.
      String baseType = Optional.ofNullable( base.getType()).orElse( additional.getType());
      String additionalType = Optional.ofNullable( additional.getType()).orElse( base.getType());

      if( !Objects.equals( baseType, additionalType))
        {
        throw
          new IllegalStateException(
            String.format(
              "Can't combine schema of type=%s with base schema of type=%s",
              additionalType,
              baseType));
        }

      combined = 
        "object".equals( baseType)?
        combineObjectSchemas( context, base, additional) :
      
        "string".equals( baseType)?
        combineStringSchemas( context, base, additional) :
      
        "integer".equals( baseType)?
        combineIntegerSchemas( context, base, additional) :
      
        "boolean".equals( baseType)?
        combineBooleanSchemas( context, base, additional) :

        "array".equals( baseType)?
        combineArraySchemas( context, base, additional) :
      
        "number".equals( baseType)?
        combineNumberSchemas( context, base, additional) :

        combineGenericSchemas( context, base, additional);
      }

    return combined;
    }
    

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  private static Schema<?> combineObjectSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineGenericSchemas( context, base, additional);

    // Combine additionalProperties
    Schema<?> baseExtraSchema =
      Optional.ofNullable( base.getAdditionalProperties())
      .map( Object::getClass)
      .filter( type -> !type.equals( Boolean.class))
      .map( type -> (Schema<?>) base.getAdditionalProperties())
      .orElse( null);

    Schema<?> additionalExtraSchema =
      Optional.ofNullable( additional.getAdditionalProperties())
      .map( Object::getClass)
      .filter( type -> !type.equals( Boolean.class))
      .map( type -> (Schema<?>) additional.getAdditionalProperties())
      .orElse( null);

    if( baseExtraSchema != null)
      {
      combined.setAdditionalProperties(
        additionalExtraSchema != null?
        (Object) context.resultFor( "additionalProperties", () -> combineSchemas( context, baseExtraSchema, additionalExtraSchema)) :

        !Boolean.FALSE.equals( additional.getAdditionalProperties())?
        baseExtraSchema :

        Boolean.FALSE);
      }
    else if( additionalExtraSchema != null)
      {
      combined.setAdditionalProperties(
        !Boolean.FALSE.equals( base.getAdditionalProperties())?
        additionalExtraSchema :

        Boolean.FALSE);
      }
    else
      {
      combined.setAdditionalProperties(
        base.getAdditionalProperties() == null?
        additional.getAdditionalProperties() :

        additional.getAdditionalProperties() == null?
        base.getAdditionalProperties() :
        
        combineAssertions(
          "additionalProperties: %s",
          (Boolean) base.getAdditionalProperties(),
          (Boolean) additional.getAdditionalProperties()));
      }

    // Combine maxProperties
    combined.setMaxProperties(
      base.getMaxProperties() == null?
      additional.getMaxProperties() :

      additional.getMaxProperties() == null?
      base.getMaxProperties() :

      base.getMaxProperties().compareTo( additional.getMaxProperties()) < 0?
      base.getMaxProperties() :

      additional.getMaxProperties());

    // Combine minProperties
    combined.setMinProperties(
      base.getMinProperties() == null?
      additional.getMinProperties() :

      additional.getMinProperties() == null?
      base.getMinProperties() :

      base.getMinProperties().compareTo( additional.getMinProperties()) > 0?
      base.getMinProperties() :

      additional.getMinProperties());

    // Combine required
    List<String> combinedRequired =
      Stream.concat(
        Optional.ofNullable( base.getRequired()).map( required -> required.stream()).orElse( Stream.empty()),
        Optional.ofNullable( additional.getRequired()).map( required -> required.stream()).orElse( Stream.empty()))
      .collect(
        () -> new LinkedHashSet<String>(),
        (set, property) -> set.add( property),
        (set, other) -> set.addAll( other))
      .stream().collect( toList());
    combined.setRequired( combinedRequired);

    // Combine not required
    setNotRequired( combined, getNotRequired( base));
    addNotRequired( combined, getNotRequired( additional));

    Optional.ofNullable( getNotRequired( combined))
      .flatMap( notRequired -> {
        return
          combinedRequired.stream()
          .filter( property -> notRequired.contains( property))
          .findFirst();
        })
      .ifPresent( property -> {
        throw
          new IllegalStateException(
            String.format(
              "Can't combine schema requiring {required: [%s]} with schema requiring {not: {required: [%s]}}",
              property,
              property));
        });
    
    // Combine properties
    Map<String,Schema> basePropertyDefs = Optional.ofNullable( base.getProperties()).orElse( emptyMap());
    Map<String,Schema> additionalPropertyDefs = Optional.ofNullable( additional.getProperties()).orElse( emptyMap());
    Map<String,Schema> combinedPropertyDefs =
      context.resultFor(
        "properties",
        () -> 
        Stream.concat( basePropertyDefs.keySet().stream(), additionalPropertyDefs.keySet().stream())
        .collect( toSet())
        .stream()
        .collect(
          () -> new LinkedHashMap<String,Schema>(),
          (map, p) -> map.put( p, context.resultFor( p, () -> combineSchemas( context, basePropertyDefs.get( p), additionalPropertyDefs.get( p)))),
          (map, other) -> map.putAll( other)));
    combined.setProperties( combinedPropertyDefs);
  
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  private static Schema<?> combineStringSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineGenericSchemas( context, base, additional);

    // Combine format
    if( base.getFormat() != null && additional.getFormat() != null && !base.getFormat().equals( additional.getFormat()))
      {
      throw
        new IllegalStateException(
          String.format(
            "format=%s is not consistent with base format=%s",
            additional.getFormat(),
            base.getFormat()));
      }
    
    // Combine maxLength
    combined.setMaxLength(
      base.getMaxLength() == null?
      additional.getMaxLength() :

      additional.getMaxLength() == null?
      base.getMaxLength() :

      base.getMaxLength().compareTo( additional.getMaxLength()) < 0?
      base.getMaxLength() :

      additional.getMaxLength());

    // Combine minLength
    combined.setMinLength(
      base.getMinLength() == null?
      additional.getMinLength() :

      additional.getMinLength() == null?
      base.getMinLength() :

      base.getMinLength().compareTo( additional.getMinLength()) > 0?
      base.getMinLength() :

      additional.getMinLength());

    // Combine pattern
    setPatterns( combined, getPatterns( base));
    addPatterns( combined, getPatterns( additional));

    // Combine not patterns
    setNotPatterns( combined, getNotPatterns( base));
    addNotPatterns( combined, getNotPatterns( additional));
      
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  private static Schema<?> combineIntegerSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineNumericSchemas( context, base, additional);

    // Combine format
    combined.setFormat(
      base.getFormat() == null?
      additional.getFormat() :

      additional.getFormat() == null?
      base.getFormat() :

      base.getFormat().equals( "int32")?
      base.getFormat() :

      additional.getFormat());
    
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  private static Schema<?> combineBooleanSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    return combineGenericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  private static Schema<?> combineArraySchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    ArraySchema combined = combineGenericSchemas( context, new ArraySchema(), base, additional);

    // Combine maxItems
    combined.setMaxItems(
      base.getMaxItems() == null?
      additional.getMaxItems() :

      additional.getMaxItems() == null?
      base.getMaxItems() :

      base.getMaxItems().compareTo( additional.getMaxItems()) < 0?
      base.getMaxItems() :

      additional.getMaxItems());

    // Combine minItems
    combined.setMinItems(
      base.getMinItems() == null?
      additional.getMinItems() :

      additional.getMinItems() == null?
      base.getMinItems() :

      base.getMinItems().compareTo( additional.getMinItems()) > 0?
      base.getMinItems() :

      additional.getMinItems());

    // Combine uniqueItems
    combined.setUniqueItems(
      base.getUniqueItems() == null?
      additional.getUniqueItems() :

      additional.getUniqueItems() == null?
      base.getUniqueItems() :

      combineAssertions( "uniqueItems: %s", base.getUniqueItems(), additional.getUniqueItems()));

    // Combine items
    Schema<?> baseItems = Optional.ofNullable( asArraySchema( base)).map( ArraySchema::getItems).orElse( null);
    Schema<?> additionalItems = Optional.ofNullable( asArraySchema( additional)).map( ArraySchema::getItems).orElse( null);    
    combined.setItems( context.resultFor( "items", () -> combineSchemas( context, baseItems, additionalItems)));     

    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  private static Schema<?> combineNumberSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineNumericSchemas( context, base, additional);

    // Combine format
    combined.setFormat(
      base.getFormat() == null?
      additional.getFormat() :

      additional.getFormat() == null?
      base.getFormat() :

      base.getFormat().equals( "float")?
      base.getFormat() :

      additional.getFormat());
    
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  private static Schema<?> combineNumericSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineGenericSchemas( context, base, additional);

    // Combine maximum
    combined.setMaximum(
      base.getMaximum() == null?
      additional.getMaximum() :

      additional.getMaximum() == null?
      base.getMaximum() :

      base.getMaximum().compareTo( additional.getMaximum()) < 0?
      base.getMaximum() :

      additional.getMaximum());
    
    // Combine minimum
    combined.setMinimum(
      base.getMinimum() == null?
      additional.getMinimum() :

      additional.getMinimum() == null?
      base.getMinimum() :

      base.getMinimum().compareTo( additional.getMinimum()) > 0?
      base.getMinimum() :

      additional.getMinimum());
    
    // Combine exclusiveMaximum
    combined.setExclusiveMaximum(
      base.getExclusiveMaximum() == null?
      additional.getExclusiveMaximum() :

      additional.getExclusiveMaximum() == null?
      base.getExclusiveMaximum() :

      combineAssertions( "exclusiveMaximum: %s", base.getExclusiveMaximum(), additional.getExclusiveMaximum()));

      
    // Combine exclusiveMinimum
    combined.setExclusiveMinimum(
      base.getExclusiveMinimum() == null?
      additional.getExclusiveMinimum() :

      additional.getExclusiveMinimum() == null?
      base.getExclusiveMinimum() :

      combineAssertions( "exclusiveMinimum: %s", base.getExclusiveMinimum(), additional.getExclusiveMinimum()));

    // Combine multipleOf
    combined.setMultipleOf(
      base.getMultipleOf() == null?
      additional.getMultipleOf() :

      additional.getMultipleOf() == null?
      base.getMultipleOf() :

      combineMultipleOf( base.getMultipleOf(), additional.getMultipleOf())); 

    // Combine not multipleOfs
    setNotMultipleOfs( combined, getNotMultipleOfs( base));
    addNotMultipleOfs( combined, getNotMultipleOfs( additional));

    return combined;
    }

  /**
   * If the given multipleOf factors are congruent, returns the maximum value.
   * Otherwise, throws an exception to report the inconsistent values.
   */
  private static BigDecimal combineMultipleOf( BigDecimal base, BigDecimal additional)
    {
    BigDecimal max = base.compareTo( additional) > 0? base : additional;
    BigDecimal min = base.compareTo( additional) < 0? base : additional;

    if( max.remainder( min).compareTo( BigDecimal.ZERO) != 0)
      {
      throw new IllegalStateException( String.format( "multipleOf=%s is not consistent with base multipleOf=%s", additional, base));
      }

    return max;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes" })
  private static Schema combineGenericSchemas( NotificationContext context, Schema base, Schema additional)
    {
    return combineGenericSchemas( context, emptySchema(), base, additional);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  private static <T extends Schema> T combineGenericSchemas( NotificationContext context, T combined, Schema base, Schema additional)
    {
    // Combine type
    String baseType = base.getType();
    String additionalType = additional.getType();

    if( baseType == null)
      {
      Optional.ofNullable( getNotTypes( base))
        .filter( notTypes -> notTypes.contains( additionalType))
        .ifPresent( notTypes -> {
          throw
            new IllegalStateException(
              String.format(
                "Can't combine schema requiring {type: [%s]} with schema requiring {not: {type: [%s]}}",
                additionalType,
                additionalType));
          });
      }
    if( additionalType == null)
      {
      Optional.ofNullable( getNotTypes( additional))
        .filter( notTypes -> notTypes.contains( baseType))
        .ifPresent( notTypes -> {
          throw
            new IllegalStateException(
              String.format(
                "Can't combine schema requiring {type: [%s]} with schema requiring {not: {type: [%s]}}",
                baseType,
                baseType));
          });
      }

    combined.setType(
      Optional.ofNullable( baseType)
      .orElse( additionalType));

    if( combined.getType() == null)
      {
      setNotTypes( combined, getNotTypes( base));
      addNotTypes( combined, getNotTypes( additional));
      }

    // Combine default
    combined.setDefault(
      Optional.ofNullable( additional.getDefault())
      .orElse( base.getDefault()));

    // Combine format
    combined.setFormat(
      base.getFormat() == null?
      additional.getFormat() :

      additional.getFormat() == null?
      base.getFormat() :

      additional.getFormat());

    // Combine enum
    combined.setEnum(
      base.getEnum() == null?
      additional.getEnum() :

      additional.getEnum() == null?
      base.getEnum() :

      Optional.of( base.getEnum())
      .map( enums -> {
          Set<Object> baseEnums = new LinkedHashSet<Object>( enums);
          baseEnums.retainAll( additional.getEnum());
          return baseEnums;
        })
      .filter( enums -> !enums.isEmpty())
      .map( enums -> enums.stream().collect( toList()))
      .orElseThrow( () -> new IllegalStateException( String.format( "enum=%s is not consistent with base enum=%s", additional.getEnum(), base.getEnum()))));

    // Combine not enums
    setNotEnums( combined, getNotEnums( base));
    addNotEnums( combined, getNotEnums( additional));
    
    // Combine nullable
    combined.setNullable(
      base.getNullable() == null?
      additional.getNullable() :

      additional.getNullable() == null?
      base.getNullable() :

      combineAssertions( "nullable: %s", base.getNullable(), additional.getNullable()));


    // Combine readOnly
    combined.setReadOnly(
      base.getReadOnly() == null?
      additional.getReadOnly() :

      additional.getReadOnly() == null?
      base.getReadOnly() :

      combineAssertions( "readOnly: %s", base.getReadOnly(), additional.getReadOnly()));

    // Combine writeOnly
    combined.setWriteOnly(
      base.getWriteOnly() == null?
      additional.getWriteOnly() :

      additional.getWriteOnly() == null?
      base.getWriteOnly() :

      combineAssertions( "writeOnly: %s", base.getWriteOnly(), additional.getWriteOnly()));

    if( Boolean.TRUE.equals( combined.getReadOnly()) && Boolean.TRUE.equals( combined.getWriteOnly()))
      {
      String baseProp = Boolean.TRUE.equals( base.getReadOnly())? "readOnly" : "writeOnly";
      String additionalProp = Boolean.TRUE.equals( additional.getReadOnly())? "readOnly" : "writeOnly";
      throw
        new IllegalStateException(
          String.format(
            "Can't combine schema requiring %s=true with base schema requiring %s=true",
            additionalProp,
            baseProp));
      }
    
    return combined;
    }

  /**
   * Returns the combination of the given boolean assertions. 
   */
  private static Boolean combineAssertions( String assertionFormat, boolean base, boolean additional)
    {
    if( base != additional)
      {
      throw
        new IllegalStateException(
          String.format(
            "Can't combine schema requiring {" + assertionFormat + "} with base schema requiring {" + assertionFormat + "}",
            additional,
            base));
      }

    return base;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  public static List<Schema<?>> not( Schema<?> schema)
    {
    String type = Optional.ofNullable( schema).map( Schema::getType).orElse( null);;

    List<Schema<?>> alternatives = 

      schema == null?
      new ArrayList<Schema<?>>() :

      "object".equals( type)?
      notObject( schema) :
      
      "string".equals( type)?
      notString( schema) :
      
      "integer".equals( type)?
      notInteger( schema) :
      
      "boolean".equals( type)?
      notBoolean( schema) :

      "array".equals( type)?
      notArray( schema) :
      
      "number".equals( type)?
      notNumber( schema) :

      notGeneric( schema);

    // Empty schema?
    if( alternatives.isEmpty())
      {
      // Yes, return an alternative schema that will invalidate any instance.
      alternatives.add(
        assertNot(
          emptySchema(),
          SCHEMA_TYPES,
          (s,v) -> { setNotTypes( s, v); s.setNullable( false); }));
      }

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  @SuppressWarnings({ "rawtypes" })
  private static List<Schema<?>> notObject( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

    // Not maxProperties
    Optional.ofNullable( schema.getMaxProperties())
      .map( maxProperties -> assertNot( schema, maxProperties, (s,v) -> s.setMinProperties( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minProperties
    Optional.ofNullable( schema.getMinProperties())
      .filter( minProperties -> minProperties > 0)
      .map( minProperties -> assertNot( schema, minProperties, (s,v) -> s.setMaxProperties( v - 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not properties
    Optional.ofNullable( schema.getProperties()).orElse( emptyMap())
      .forEach( (property, propertySchema) -> {

        List<String> notRequired =
          Optional.ofNullable( schema.getRequired()).map( required -> required.contains( property)).orElse( false)
          ? null
          : Arrays.asList( property);

        not( propertySchema)
          .stream()
          .map(
            notSchema ->
            assertNot(
              schema,
              (Schema) notSchema,
              (s,v) -> { s.setProperties( MapBuilder.of( property, v).build()); s.setRequired( notRequired); }))
          .forEach( s -> alternatives.add( s));
        });

    // Not required
    Optional.ofNullable( schema.getRequired()).orElse( emptyList())
      .stream()
      .map( property -> assertNot( schema, property, (s,v) -> addNotRequired( s, v)))
      .forEach( s -> alternatives.add( s));      

    // Not additionalProperties
    Optional.ofNullable( schema.getAdditionalProperties())
      .ifPresent( ap -> {
        if( ap instanceof Schema)
          {
          not( (Schema<?>) ap).stream()
            .map( notSchema -> assertNot( schema, notSchema, (s,v) -> s.setAdditionalProperties( v)))
            .forEach( s -> alternatives.add( s));
          }
        else
          {
          alternatives.add( assertNot( schema, (Boolean) ap, (s,v) -> s.setAdditionalProperties( !v)));
          }
        });

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notString( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

    // Not maxLength
    Optional.ofNullable( schema.getMaxLength())
      .map( maxLength -> assertNot( schema, maxLength, (s,v) -> s.setMinLength( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minLength
    Optional.ofNullable( schema.getMinLength())
      .filter( minLength -> minLength > 0)
      .map( minLength -> assertNot( schema, minLength, (s,v) -> s.setMaxLength( v - 1)))
      .ifPresent( s -> alternatives.add( s));

    // Not pattern
    Optional.ofNullable( schema.getPattern())
      .map( pattern -> assertNot( schema, pattern, (s,v) -> addNotPattern( s, v)))
      .ifPresent( s -> alternatives.add( s));

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notInteger( Schema<?> schema)
    {
    return notNumeric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notBoolean( Schema<?> schema)
    {
    return notGeneric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notArray( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

    // Not maxItems
    Optional.ofNullable( schema.getMinItems())
      .filter( max -> max > 0)
      .map( max -> assertNot( schema, max, (s,v) -> s.setMaxItems( v - 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minItems
    Optional.ofNullable( schema.getMaxItems())
      .map( min -> assertNot( schema, min, (s,v) -> s.setMinItems( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not uniqueItems
    Optional.ofNullable( schema.getUniqueItems())
      .map( unique -> assertNot( schema, unique, (s,v) -> s.setUniqueItems( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not items
    Optional.ofNullable( asArraySchema( schema))
      .ifPresent(
        array -> 
        Optional.ofNullable( array.getItems())
          .ifPresent(
            items ->
            not( items).stream()
            .map( notItems -> assertNot( array, notItems, (s,v) -> s.setItems( notItems)))
            .forEach( s -> alternatives.add( s))));
    
    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notNumber( Schema<?> schema)
    {
    return notNumeric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notNumeric( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

    // Not maximum
    boolean exclusiveMaximum = !Boolean.TRUE.equals( schema.getExclusiveMinimum());
    Optional.ofNullable( schema.getMinimum())
      .map( max -> assertNot( schema, max, (s,v) -> s.setMaximum( max)))
      .ifPresent( s -> { s.setExclusiveMaximum( exclusiveMaximum); alternatives.add( s); });
    
    // Not minimum
    boolean exclusiveMinimum = !Boolean.TRUE.equals( schema.getExclusiveMaximum());
    Optional.ofNullable( schema.getMaximum())
      .map( min -> assertNot( schema, min, (s,v) -> s.setMinimum( min)))
      .ifPresent( s -> { s.setExclusiveMinimum( exclusiveMinimum); alternatives.add( s); });

    // Not exclusive maximum
    Optional.ofNullable( schema.getExclusiveMaximum())
      .map( exclusive -> assertNot( schema, exclusive, (s,v) -> s.setExclusiveMaximum( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not exclusive minimum
    Optional.ofNullable( schema.getExclusiveMinimum())
      .map( exclusive -> assertNot( schema, exclusive, (s,v) -> s.setExclusiveMinimum( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not multipleOf
    Optional.ofNullable( schema.getMultipleOf())
      .map( multipleOf -> assertNot( schema, multipleOf, (s,v) -> addNotMultipleOf( s, v)))
      .ifPresent( s -> alternatives.add( s));

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notGeneric( Schema<?> schema)
    {
    List<Schema<?>> alternatives = new ArrayList<Schema<?>>();

    // Not type
    Optional.ofNullable( schema.getType())
      .map( type -> assertNot( emptySchema(), type, (s,v) -> addNotType( s, v)))
      .ifPresent( s -> alternatives.add( s));

    // Not nullable
    Optional.ofNullable( schema.getNullable())
      .map( nullable -> assertNot( schema, nullable, (s,v) -> s.setNullable( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not enums
    Optional.ofNullable( schema.getEnum())
      .map( enums -> assertNot( schema, enums, (s,v) -> setNotEnums( s, v)))
      .ifPresent( s -> alternatives.add( s));

    // Not readOnly
    Optional.ofNullable( schema.getReadOnly())
      .map( readOnly -> assertNot( schema, readOnly, (s,v) -> s.setReadOnly( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not writeOnly
    Optional.ofNullable( schema.getWriteOnly())
      .map( writeOnly -> assertNot( schema, writeOnly, (s,v) -> s.setWriteOnly( !v)))
      .ifPresent( s -> alternatives.add( s));
    
    return alternatives;
    }

  /**
   * Returns an alternate schema that negates the assertion of the given value from the orginal schema.
   */
  @SuppressWarnings("unchecked")
  private static <S extends Schema<?>,V> Schema<?> assertNot( S original, V asserted, BiConsumer<S,V> negater)
    {
    Class<S> schemaType = (Class<S>) original.getClass();
    try
      {
      S alternative = schemaType.newInstance();
      alternative.setType( original.getType());

      negater.accept( alternative, asserted);

      return alternative;
      }
    catch( Exception e)
      {
      throw new RuntimeException( String.format( "Can't create alternative schema of type=%s", schemaType.getSimpleName()), e);
      }
    }

  /**
   * Returns a new empty schema.
   */
  public static Schema<?> emptySchema()
    {
    return new Schema<Object>();
    }

  private static final Set<String> SCHEMA_TYPES =
    Arrays.asList( "array", "boolean", "integer", "number", "object", "string")
    .stream().collect( toSet());
  }
