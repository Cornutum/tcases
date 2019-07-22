//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.SchemaExtensions.*;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import static org.apache.commons.lang3.math.NumberUtils.INTEGER_ZERO;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
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
  public static String impliedType( String type, Schema<?> schema, Function<Schema<?>,Object>... accessors)
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
  public static Schema<?> combineObjectSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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

    combined.setAdditionalProperties(
      Boolean.FALSE.equals( base.getAdditionalProperties()) || Boolean.FALSE.equals( additional.getAdditionalProperties())?
      (Object) Boolean.FALSE :

      baseExtraSchema != null && additionalExtraSchema != null?
      (Object) context.resultFor( "additionalProperties", () -> combineSchemas( context, baseExtraSchema, additionalExtraSchema)) :
      
      baseExtraSchema != null?
      (Object) baseExtraSchema :

      (Object) additionalExtraSchema);      

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
    combined.setRequired(
      Stream.concat(
        Optional.ofNullable( base.getRequired()).map( required -> required.stream()).orElse( Stream.empty()),
        Optional.ofNullable( additional.getRequired()).map( required -> required.stream()).orElse( Stream.empty()))
      .collect(
        () -> new LinkedHashSet<String>(),
        (set, property) -> set.add( property),
        (set, other) -> set.addAll( other))
      .stream().collect( toList()));

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
  public static Schema<?> combineStringSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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
      
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineIntegerSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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
  public static Schema<?> combineBooleanSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    return combineGenericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineArraySchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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
      Boolean.TRUE.equals( base.getUniqueItems())
      ? base.getUniqueItems() 
      : additional.getUniqueItems());     

    // Combine items
    Schema<?> baseItems = base instanceof ArraySchema? ((ArraySchema) base).getItems() : null;
    Schema<?> additionalItems = additional instanceof ArraySchema? ((ArraySchema) additional).getItems() : null;
    combined.setItems( context.resultFor( "items", () -> combineSchemas( context, baseItems, additionalItems)));     

    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineNumberSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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
  public static Schema<?> combineNumericSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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
      combined.getMaximum() == null?
      null :

      Objects.equals( combined.getMaximum(), base.getMaximum())?
      base.getExclusiveMaximum() :
 
      additional.getExclusiveMaximum());

      
    // Combine exclusiveMinimum
    combined.setExclusiveMinimum(
      combined.getMinimum() == null?
      null :

      Objects.equals( combined.getMinimum(), base.getMinimum())?
      base.getExclusiveMinimum() :
 
      additional.getExclusiveMinimum());

    // Combine multipleOf
    combined.setMultipleOf(
      base.getMultipleOf() == null?
      additional.getMultipleOf() :

      additional.getMultipleOf() == null?
      base.getMultipleOf() :

      combineMultipleOf( base.getMultipleOf(), additional.getMultipleOf())); 
    
    return combined;
    }

  /**
   * If the given multipleOf factors are congruent, returns the maximum value.
   * Otherwise, throws an exception to report the inconsistent values.
   */
  public static BigDecimal combineMultipleOf( BigDecimal base, BigDecimal additional)
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
  public static Schema combineGenericSchemas( NotificationContext context, Schema base, Schema additional)
    {
    return combineGenericSchemas( context, new Schema<Object>(), base, additional);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static <T extends Schema> T combineGenericSchemas( NotificationContext context, T combined, Schema base, Schema additional)
    {
    // Combine type
    combined.setType(
      Optional.ofNullable( base.getType())
      .orElse( additional.getType()));

    // Combine default
    combined.setDefault(
      Optional.ofNullable( additional.getDefault())
      .orElse( base.getDefault()));
    
    // Combine not
    setNots( combined, getNots( base));
    addNots( combined, getNots( additional));

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

    // Combine nullable
    combined.setNullable(
      Boolean.TRUE.equals( base.getNullable())
      ? additional.getNullable()
      : base.getNullable());

    // Combine readOnly
    combined.setReadOnly(
      Boolean.TRUE.equals( base.getReadOnly())
      ? base.getReadOnly() 
      : additional.getReadOnly());

    // Combine writeOnly
    combined.setWriteOnly(
      Boolean.TRUE.equals( base.getWriteOnly())
      ? base.getWriteOnly() 
      : additional.getWriteOnly());

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
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineNotSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
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
        combineNotObjectSchemas( context, base, additional) :
      
        "string".equals( baseType)?
        combineNotStringSchemas( context, base, additional) :
      
        "integer".equals( baseType)?
        combineNotIntegerSchemas( context, base, additional) :
      
        "boolean".equals( baseType)?
        combineNotBooleanSchemas( context, base, additional) :

        "array".equals( baseType)?
        combineNotArraySchemas( context, base, additional) :
      
        "number".equals( baseType)?
        combineNotNumberSchemas( context, base, additional) :

        combineNotGenericSchemas( context, base, additional);
      }

    return combined;
    }
    

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static Schema<?> combineNotObjectSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineNotGenericSchemas( context, base, additional);

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

    if( baseExtraSchema != null && additionalExtraSchema != null)
      {
      combined.setAdditionalProperties(
        (Object) context.resultFor( "additionalProperties", () -> combineNotSchemas( context, baseExtraSchema, additionalExtraSchema)));
      }
    else if( baseExtraSchema != null)
      {
      combined.setAdditionalProperties(
        Boolean.TRUE.equals( additional.getAdditionalProperties())
        ? Boolean.TRUE
        : baseExtraSchema);
      }
    else if( additionalExtraSchema != null)
      {
      combined.setAdditionalProperties(
        Boolean.TRUE.equals( base.getAdditionalProperties())
        ? Boolean.TRUE
        : additionalExtraSchema);
      }
    else
      {
      combined.setAdditionalProperties(
        base.getAdditionalProperties() == null?
        additional.getAdditionalProperties() :

        additional.getAdditionalProperties() == null?
        base.getAdditionalProperties() :

        combineAssertions( "not: {additionalProperties: %s}", (Boolean) base.getAdditionalProperties(), (Boolean) additional.getAdditionalProperties()));
      }
    
    // Combine maxProperties
    combined.setMaxProperties(
      base.getMaxProperties() == null?
      additional.getMaxProperties() :

      additional.getMaxProperties() == null?
      base.getMaxProperties() :

      base.getMaxProperties().compareTo( additional.getMaxProperties()) > 0?
      base.getMaxProperties() :

      additional.getMaxProperties());

    // Combine minProperties
    if( !unNegatable( context, "minProperties", INTEGER_ZERO, base.getMinProperties(), additional.getMinProperties()))
      {
      combined.setMinProperties(
        base.getMinProperties() == null?
        additional.getMinProperties() :

        additional.getMinProperties() == null?
        base.getMinProperties() :

        base.getMinProperties().compareTo( additional.getMinProperties()) < 0?
        base.getMinProperties() :

        additional.getMinProperties());
      }

    // Combine required
    combined.setRequired(
      Stream.concat(
        Optional.ofNullable( base.getRequired()).map( required -> required.stream()).orElse( Stream.empty()),
        Optional.ofNullable( additional.getRequired()).map( required -> required.stream()).orElse( Stream.empty()))
      .collect(
        () -> new LinkedHashSet<String>(),
        (set, property) -> set.add( property),
        (set, other) -> set.addAll( other))
      .stream().collect( toList()));

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
          (map, p) -> map.put( p, context.resultFor( p, () -> combineNotSchemas( context, basePropertyDefs.get( p), additionalPropertyDefs.get( p)))),
          (map, other) -> map.putAll( other)));
    combined.setProperties( combinedPropertyDefs);
  
    return combined;
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineNotStringSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineNotGenericSchemas( context, base, additional);

    // Combine maxLength
    combined.setMaxLength(
      base.getMaxLength() == null?
      additional.getMaxLength() :

      additional.getMaxLength() == null?
      base.getMaxLength() :

      base.getMaxLength().compareTo( additional.getMaxLength()) > 0?
      base.getMaxLength() :

      additional.getMaxLength());

    // Combine minLength
    if( !unNegatable( context, "minLength", INTEGER_ZERO, base.getMinLength(), additional.getMinLength()))
      {
      combined.setMinLength(
        base.getMinLength() == null?
        additional.getMinLength() :

        additional.getMinLength() == null?
        base.getMinLength() :

        base.getMinLength().compareTo( additional.getMinLength()) < 0?
        base.getMinLength() :

        additional.getMinLength());
      }

    // Combine pattern
    setPatterns( combined, getPatterns( base));
    addPatterns( combined, getPatterns( additional));
      
    return combined;
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineNotIntegerSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    return combineNotNumericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineNotBooleanSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    return combineNotGenericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineNotArraySchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    ArraySchema combined = combineNotGenericSchemas( context, new ArraySchema(), base, additional);

    // Combine maxItems
    combined.setMaxItems(
      base.getMaxItems() == null?
      additional.getMaxItems() :

      additional.getMaxItems() == null?
      base.getMaxItems() :

      base.getMaxItems().compareTo( additional.getMaxItems()) > 0?
      base.getMaxItems() :

      additional.getMaxItems());

    // Combine minItems
    if( !unNegatable( context, "minItems", INTEGER_ZERO, base.getMinItems(), additional.getMinItems()))
      {
      combined.setMinItems(
        base.getMinItems() == null?
        additional.getMinItems() :

        additional.getMinItems() == null?
        base.getMinItems() :

        base.getMinItems().compareTo( additional.getMinItems()) < 0?
        base.getMinItems() :

        additional.getMinItems());
      }

    // Combine uniqueItems
    if( !unNegatable( context, "uniqueItems", Boolean.FALSE, base.getUniqueItems(), additional.getUniqueItems()))
      {
      combined.setUniqueItems(
        base.getUniqueItems() == null?
        additional.getUniqueItems() :

        additional.getUniqueItems() == null?
        base.getUniqueItems() :

        combineAssertions( "not: {uniqueItems: %s}", base.getUniqueItems(), additional.getUniqueItems()));
      }     

    // Combine items
    Schema<?> baseItems = base instanceof ArraySchema? ((ArraySchema) base).getItems() : null;
    Schema<?> additionalItems = additional instanceof ArraySchema? ((ArraySchema) additional).getItems() : null;
    combined.setItems( context.resultFor( "items", () -> combineNotSchemas( context, baseItems, additionalItems)));     

    return combined;
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineNotNumberSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    return combineNotNumericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineNotNumericSchemas( NotificationContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineNotGenericSchemas( context, base, additional);

    // Combine maximum
    combined.setMaximum(
      base.getMaximum() == null?
      additional.getMaximum() :

      additional.getMaximum() == null?
      base.getMaximum() :

      base.getMaximum().compareTo( additional.getMaximum()) > 0?
      base.getMaximum() :

      additional.getMaximum());
    
    // Combine minimum
    combined.setMinimum(
      base.getMinimum() == null?
      additional.getMinimum() :

      additional.getMinimum() == null?
      base.getMinimum() :

      base.getMinimum().compareTo( additional.getMinimum()) < 0?
      base.getMinimum() :

      additional.getMinimum());
    
    // Combine exclusiveMaximum
    combined.setExclusiveMaximum(
      base.getExclusiveMaximum() == null?
      additional.getExclusiveMaximum() :

      additional.getExclusiveMaximum() == null?
      base.getExclusiveMaximum() :

      combineAssertions( "not: {exclusiveMaximum: %s}", base.getExclusiveMaximum(), additional.getExclusiveMaximum()));
      
    // Combine exclusiveMinimum
    combined.setExclusiveMinimum(
      base.getExclusiveMinimum() == null?
      additional.getExclusiveMinimum() :

      additional.getExclusiveMinimum() == null?
      base.getExclusiveMinimum() :

      combineAssertions( "not: {exclusiveMinimum: %s}", base.getExclusiveMinimum(), additional.getExclusiveMinimum()));

    // Combine multipleOf
    Set<BigDecimal> baseNotMultipleOfs = getNotMultipleOfs( withNotMultipleOfs( base));
    Set<BigDecimal> additionalNotMultipleOfs = getNotMultipleOfs( withNotMultipleOfs( additional));

    setNotMultipleOfs(
      combined,
      baseNotMultipleOfs
      .stream()
      .filter( baseMultipleOf -> additionalNotMultipleOfs.stream().noneMatch( additionalMultipleOf -> isMultipleOf( baseMultipleOf, additionalMultipleOf)))
      .collect( toSet()));

    addNotMultipleOfs(
      combined,
      additionalNotMultipleOfs
      .stream()
      .filter( additionalMultipleOf -> getNotMultipleOfs( combined).stream().noneMatch( baseMultipleOf -> isMultipleOf( additionalMultipleOf, baseMultipleOf)))
      .collect( toSet()));

    return combined;
    }

  /**
   * Returns true if the base value is a multiple of the additional value.
   */
  public static boolean isMultipleOf( BigDecimal base, BigDecimal additional)
    {
    return
      base.compareTo( additional) >= 0
      &&
      base.remainder( additional).compareTo( BigDecimal.ZERO) == 0;
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes" })
  public static Schema combineNotGenericSchemas( NotificationContext context, Schema base, Schema additional)
    {
    return combineNotGenericSchemas( context, new Schema<Object>(), base, additional);
    }

  /**
   * Returns a new schema that is NOT satisfied by any instance that does NOT satisfy either the base schema or the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static <T extends Schema> T combineNotGenericSchemas( NotificationContext context, T combined, Schema base, Schema additional)
    {
    // Combine type
    combined.setType(
      Optional.ofNullable( base.getType())
      .orElse( additional.getType()));
    
    // Combine format
    setNotFormats( combined, getNotFormats( withNotFormats( base)));
    addNotFormats( combined, getNotFormats( withNotFormats( additional)));

    // Combine enum
    Stream<Object> enums =
      Stream.concat(
        Optional.ofNullable( base.getEnum()).orElse( emptyList()).stream(),
        Optional.ofNullable( additional.getEnum()).orElse( emptyList()).stream());
    combined.setEnum(
      enums
      .collect( toSet())
      .stream()
      .collect( toList()));

    // Combine nullable
    if( !unNegatable( context, "nullable", Boolean.TRUE, base.getNullable(), additional.getNullable()))
      {
      combined.setNullable(
        base.getNullable() == null?
        additional.getNullable() :

        additional.getNullable() == null?
        base.getNullable() :

        combineAssertions( "not: {nullable: %s}", base.getNullable(), additional.getNullable()));
      }

    // Combine readOnly
    combined.setReadOnly(
      base.getReadOnly() == null?
      additional.getReadOnly() :

      additional.getReadOnly() == null?
      base.getReadOnly() :

      combineAssertions( "not: {readOnly: %s}", base.getReadOnly(), additional.getReadOnly()));

    // Combine writeOnly
    combined.setWriteOnly(
      base.getWriteOnly() == null?
      additional.getWriteOnly() :

      additional.getWriteOnly() == null?
      base.getWriteOnly() :

      combineAssertions( "not: {writeOnly: %s}", base.getWriteOnly(), additional.getWriteOnly()));
    
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  public static Schema<?> mergeSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    Schema<?> merged;

    if( base == null || not == null)
      {
      merged = base;
      }
    else
      {
      // A null type indicates an empty schema, which can be merged with any type.
      String baseType = Optional.ofNullable( base.getType()).orElse( not.getType());
      String notType = Optional.ofNullable( not.getType()).orElse( base.getType());

      if( !Objects.equals( baseType, notType))
        {
        throw
          new IllegalStateException(
            String.format(
              "Can't merge \"not\" schema of type=%s with base schema of type=%s",
              notType,
              baseType));
        }

      merged =
        "object".equals( baseType)?
        mergeObjectSchemas( context, base, not) :
      
        "string".equals( baseType)?
        mergeStringSchemas( context, base, not) :
      
        "integer".equals( baseType)?
        mergeIntegerSchemas( context, base, not) :
      
        "boolean".equals( baseType)?
        mergeBooleanSchemas( context, base, not) :

        "array".equals( baseType)?
        mergeArraySchemas( context, base, not) :
      
        "number".equals( baseType)?
        mergeNumberSchemas( context, base, not) :

        mergeGenericSchemas( context, base, not);
      }
    
    return merged;
    }    

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static Schema<?> mergeObjectSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    Schema merged = mergeGenericSchemas( context, base, not);

    // Merge additionalProperties
    Schema<?> baseExtraSchema =
      Optional.ofNullable( base.getAdditionalProperties())
      .map( Object::getClass)
      .filter( type -> !type.equals( Boolean.class))
      .map( type -> (Schema<?>) base.getAdditionalProperties())
      .orElse( null);

    Schema<?> notExtraSchema =
      Optional.ofNullable( not.getAdditionalProperties())
      .map( Object::getClass)
      .filter( type -> !type.equals( Boolean.class))
      .map( type -> (Schema<?>) not.getAdditionalProperties())
      .orElse( null);

    if( baseExtraSchema != null && Boolean.TRUE.equals( not.getAdditionalProperties()))
      {
      throw unmergeableValue( "additionalProperties", Boolean.TRUE);
      }

    merged.setAdditionalProperties(
      baseExtraSchema == null && notExtraSchema == null?
      mergeAssertions( "additionalProperties", (Boolean) base.getAdditionalProperties(), (Boolean) not.getAdditionalProperties()) :

      base.getAdditionalProperties());

    if( notExtraSchema != null && !Boolean.FALSE.equals( base.getAdditionalProperties()))
      {
      setNotAdditionalProperties( merged, notExtraSchema);
      }

    // Merge maxProperties
    Integer exclusiveMinProperties = not.getMaxProperties();
    Integer exclusiveMaxProperties = adjustedExclusiveMaxOf( context, "Properties", exclusiveMinProperties, not.getMinProperties());

    merged.setMaxProperties(
      exclusiveMaxProperties == null?
      base.getMaxProperties() :

      base.getMaxProperties() == null?
      Integer.valueOf( exclusiveMaxProperties - 1) :

      base.getMaxProperties().compareTo( exclusiveMaxProperties) < 0?
      base.getMaxProperties() :

      Integer.valueOf( exclusiveMaxProperties - 1));

    // Merge minProperties
    merged.setMinProperties(
      exclusiveMinProperties == null?
      base.getMinProperties() :

      base.getMinProperties() == null?
      Integer.valueOf( exclusiveMinProperties + 1) :

      base.getMinProperties().compareTo( exclusiveMinProperties) > 0?
      base.getMinProperties() :

      Integer.valueOf( exclusiveMinProperties + 1));

    // Merge required
    List<String> notRequired = Optional.ofNullable( not.getRequired()).orElse( emptyList());
    merged.setRequired(
      Optional.ofNullable( base.getRequired()).orElse( emptyList())
      .stream()
      .filter( baseRequired -> !notRequired.contains( baseRequired))
      .collect( toList()));

    // Merge properties
    merged.setProperties( base.getProperties());
    setNotProperties( merged, not.getProperties());
  
    return merged;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> mergeStringSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    Schema merged = mergeGenericSchemas( context, base, not);

    // Merge maxLength
    Integer exclusiveMinLength = not.getMaxLength();
    Integer exclusiveMaxLength = adjustedExclusiveMaxOf( context, "Length", exclusiveMinLength, not.getMinLength());

    merged.setMaxLength(
      exclusiveMaxLength == null?
      base.getMaxLength() :

      base.getMaxLength() == null?
      Integer.valueOf( exclusiveMaxLength - 1) :

      base.getMaxLength().compareTo( exclusiveMaxLength) < 0?
      base.getMaxLength() :

      Integer.valueOf( exclusiveMaxLength - 1));

    // Merge minLength
    merged.setMinLength(
      exclusiveMinLength == null?
      base.getMinLength() :

      base.getMinLength() == null?
      Integer.valueOf( exclusiveMinLength + 1) :

      base.getMinLength().compareTo( exclusiveMinLength) > 0?
      base.getMinLength() :

      Integer.valueOf( exclusiveMinLength + 1));

    // Merge pattern
    Set<String> basePatterns = getPatterns( base);
    Set<String> notPatterns = getPatterns( not);
    basePatterns
      .stream()
      .filter( basePattern -> notPatterns.contains( basePattern))
      .findFirst()
      .ifPresent( basePattern -> { throw unmergeableStringValue( "pattern", basePattern);});

    setPatterns( merged, basePatterns);
    setNotPatterns( merged, notPatterns);
      
    return merged;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  public static Schema<?> mergeIntegerSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    return mergeNumericSchemas( context, base, not);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  public static Schema<?> mergeBooleanSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    return mergeGenericSchemas( context, base, not);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  public static Schema<?> mergeArraySchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    ArraySchema merged = mergeGenericSchemas( context, new ArraySchema(), base, not);

    // Merge maxItems
    Integer exclusiveMinItems = not.getMaxItems();
    Integer exclusiveMaxItems = adjustedExclusiveMaxOf( context, "Items", exclusiveMinItems, not.getMinItems());

    merged.setMaxItems(
      exclusiveMaxItems == null?
      base.getMaxItems() :

      base.getMaxItems() == null?
      Integer.valueOf( exclusiveMaxItems - 1) :

      base.getMaxItems().compareTo( exclusiveMaxItems) < 0?
      base.getMaxItems() :

      Integer.valueOf( exclusiveMaxItems - 1));

    // Merge minItems
    merged.setMinItems(
      exclusiveMinItems == null?
      base.getMinItems() :

      base.getMinItems() == null?
      Integer.valueOf( exclusiveMinItems + 1) :

      base.getMinItems().compareTo( exclusiveMinItems) > 0?
      base.getMinItems() :

      Integer.valueOf( exclusiveMinItems + 1));

    // Merge uniqueItems
    merged.setUniqueItems( mergeAssertions( "uniqueItems", base.getUniqueItems(), not.getUniqueItems()));

    // Merge items
    Schema<?> baseItems = base instanceof ArraySchema? ((ArraySchema) base).getItems() : null;
    Schema<?> notItems = not instanceof ArraySchema? ((ArraySchema) not).getItems() : null;
    merged.setItems( baseItems);
    setNotItems( merged, notItems);

    return merged;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  public static Schema<?> mergeNumberSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    return mergeNumericSchemas( context, base, not);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> mergeNumericSchemas( NotificationContext context, Schema<?> base, Schema<?> not)
    {
    Schema merged = mergeGenericSchemas( context, base, not);

    // Merge maximum
    BigDecimal exclusiveMinimum = not.getMaximum();
    BigDecimal exclusiveMaximum = not.getMinimum();

    merged.setMaximum(
      exclusiveMaximum == null?
      base.getMaximum() :

      base.getMaximum() == null?
      exclusiveMaximum.subtract( exclusiveMaximum.ulp()) :

      base.getMaximum().compareTo( exclusiveMaximum) < 0?
      base.getMaximum() :

      exclusiveMaximum.subtract( exclusiveMaximum.ulp()));

    // Merge minimum
    merged.setMinimum(
      exclusiveMinimum == null?
      base.getMinimum() :

      base.getMinimum() == null?
      exclusiveMinimum.add( exclusiveMinimum.ulp()) :

      base.getMinimum().compareTo( exclusiveMinimum) > 0?
      base.getMinimum() :

      exclusiveMinimum.add( exclusiveMinimum.ulp()));
    
    // Merge exclusiveMaximum
    merged.setExclusiveMaximum(
      merged.getMaximum() == null
      ? null
      : mergeAssertions( "exclusiveMaximum", base.getExclusiveMaximum(), not.getExclusiveMaximum()));
      
    // Merge exclusiveMinimum
    merged.setExclusiveMinimum(
      merged.getMinimum() == null
      ? null
      : mergeAssertions( "exclusiveMinimum", base.getExclusiveMinimum(), not.getExclusiveMinimum()));

    // Merge multipleOf
    BigDecimal baseMultipleOf = base.getMultipleOf();
    Set<BigDecimal> notMultipleOfs = getNotMultipleOfs( withNotMultipleOfs( not));
    if( baseMultipleOf != null && notMultipleOfs.stream().anyMatch( notMultipleOf -> isMultipleOf( baseMultipleOf, notMultipleOf)))
      {
      throw unmergeableValue( "multipleOf", baseMultipleOf);
      }
    merged.setMultipleOf( base.getMultipleOf());
    setNotMultipleOfs( merged, notMultipleOfs);
    
    return merged;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  @SuppressWarnings({ "rawtypes" })
  public static Schema mergeGenericSchemas( NotificationContext context, Schema base, Schema not)
    {
    return mergeGenericSchemas( context, new Schema<Object>(), base, not);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the not schema.
   * Throws an exception if a consistent result is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static <T extends Schema> T mergeGenericSchemas( NotificationContext context, T merged, Schema base, Schema not)
    {
    // Merge type
    merged.setType(
      Optional.ofNullable( base.getType())
      .orElse( not.getType()));

    // Merge default
    merged.setDefault( base.getDefault());
    
    // Merge format
    if( getNotFormats( withNotFormats( not)).stream().anyMatch( notFormat -> Objects.equals( base.getFormat(), notFormat)))
      {
      throw unmergeableStringValue( "format", base.getFormat());
      }
    if( base.getFormat() == null)
      {
      setNotFormats( merged, getNotFormats( not));
      }
    else
      {
      merged.setFormat( base.getFormat());
      }
    
    // Merge enum
    List<Object> baseEnums = Optional.ofNullable( base.getEnum()).orElse( emptyList());
    List<Object> notEnums = Optional.ofNullable( not.getEnum()).orElse( emptyList());
    notEnums
      .stream()
      .filter( notEnum -> baseEnums.contains( notEnum))
      .findFirst()
      .ifPresent( notEnum -> { throw unmergeableValue( "enum", notEnum); });

    if( baseEnums.isEmpty())
      {
      setNotEnums( merged, notEnums);
      }
    else
      {
      merged.setEnum( baseEnums);
      }

    // Merge nullable
    merged.setNullable( mergeAssertions( "nullable", base.getNullable(), not.getNullable()));

    // Merge readOnly
    merged.setReadOnly( mergeAssertions( "readOnly", base.getReadOnly(), not.getReadOnly()));

    // Merge writeOnly
    merged.setReadOnly( mergeAssertions( "writeOnly", base.getWriteOnly(), not.getWriteOnly()));

    if( Boolean.TRUE.equals( merged.getReadOnly()) && Boolean.TRUE.equals( merged.getWriteOnly()))
      {
      String baseProp = Boolean.TRUE.equals( base.getReadOnly())? "readOnly" : "writeOnly";
      String notProp = Boolean.FALSE.equals( not.getReadOnly())? "readOnly" : "writeOnly";
      throw
        new IllegalStateException(
          String.format(
            "Can't merge \"not\" schema requiring %s=true with base schema requiring %s=true",
            notProp,
            baseProp));
      }
    
    return merged;
    }

  /**
   * Returns the adjusted exclusive maximum of the given range.
   */
  private static Integer adjustedExclusiveMaxOf( NotificationContext context, String description, Integer exclusiveMin, Integer exclusiveMax)
    {
    if( exclusiveMax != null && exclusiveMin != null && exclusiveMax - exclusiveMin < 2)
      {
      int adjusted = exclusiveMin + 2;

      context.error(
        String.format(
          "(exclusiveMin%s=%s, exclusiveMax%s=%s) is an infeasible range",
          description, exclusiveMin,
          description, exclusiveMax),
        String.format(
          "Adjusting exclusiveMax%s to %s",
          description,
          adjusted));

      exclusiveMax = adjusted;
      }

    return exclusiveMax;
    }

  /**
   * Returns an exception reporting a merge failure for the given schema keyword value.
   */
  private static RuntimeException unmergeableStringValue( String keyword, String value)
    {
    return unmergeableValue( keyword, String.format( "\"%s\"", value));
    }

  /**
   * Returns an exception reporting a merge failure for the given schema keyword value.
   */
  private static RuntimeException unmergeableValue( String keyword, Object value)
    {
    return new IllegalStateException( String.format( "{%s: %s} is not consistent with {not: {%s: %s}}", keyword, value, keyword, value));
    }

  /**
   * Returns true if either the base or the additional value equals the given non-negatable value for the given "not" schema keyword.
   */
  private static <T> boolean unNegatable( NotificationContext context, String keyword, T nonNegatable, T base, T additional)
    {
    boolean unNegatable = nonNegatable.equals( base) || nonNegatable.equals( additional);
    if( unNegatable)
      {
      context.error(
        String.format( "\"not: {%s: %s}\" assertion can't be satisfied by any instance", keyword, nonNegatable),
        "Ignoring infeasible assertion");
      }
    return unNegatable;
    }

  /**
   * Returns the given "not" schema, initializing the value of the "not formats" extension.
   */
  private static Schema<?> withNotFormats( Schema<?> notSchema)
    {
    Optional.ofNullable( notSchema.getFormat())
      .ifPresent( notFormat -> addNotFormat( notSchema, notFormat));

    return notSchema;
    }

  /**
   * Returns the given "not" schema, initializing the value of the "not multipleOfs" extension.
   */
  private static Schema<?> withNotMultipleOfs( Schema<?> notSchema)
    {
    Optional.ofNullable( notSchema.getMultipleOf())
      .ifPresent( notMultipleOf -> addNotMultipleOf( notSchema, notMultipleOf));

    return notSchema;
    }

  /**
   * Returns the combination of the given boolean assertions. 
   */
  public static Boolean combineAssertions( String assertionFormat, boolean base, boolean additional)
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
   * Returns the merger of the given boolean assertions. 
   */
  public static Boolean mergeAssertions( String keyword, Boolean base, Boolean not)
    {
    Boolean merged = null;

    if( base == null && not == null)
      {
      merged = null;
      }
    else if( base == null)
      {
      merged = !not;
      }
    else if( !Objects.equals( base, not))
      {
      merged = base;
      }
    else
      {
      throw unmergeableValue( keyword, base);
      }

    return merged;
    }

  private static final Set<String> SCHEMA_TYPES =
    Arrays.asList( "array", "boolean", "integer", "number", "object", "string")
    .stream().collect( toSet());
  }
