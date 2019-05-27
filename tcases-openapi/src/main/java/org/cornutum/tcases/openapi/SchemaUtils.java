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

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;
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
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static Schema<?> combineObjectSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
          () -> new HashMap<String,Schema>(),
          (map, p) -> map.put( p, context.resultFor( p, () -> combineSchemas( context, basePropertyDefs.get( p), additionalPropertyDefs.get( p)))),
          (map, other) -> map.putAll( other)));
    combined.setProperties( combinedPropertyDefs);
  
    return combined;
    }

  /**
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineStringSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
    addPattern( combined, additional.getPattern());
      
    return combined;
    }

  /**
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineIntegerSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineBooleanSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
    {
    return combineGenericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  public static Schema<?> combineArraySchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineNumberSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  public static Schema<?> combineNumericSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes" })
  public static Schema combineGenericSchemas( OpenApiContext context, Schema base, Schema additional)
    {
    return combineGenericSchemas( context, new Schema<Object>(), base, additional);
    }

  /**
   * Returns a new schema formed by combining the base schema with assertions from the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static <T extends Schema> T combineGenericSchemas( OpenApiContext context, T combined, Schema base, Schema additional)
    {
    combined.setType( Optional.ofNullable( base.getType()).orElse( additional.getType()));

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

  private static final Set<String> SCHEMA_TYPES =
    Arrays.asList( "array", "boolean", "integer", "number", "object", "string")
    .stream().collect( toSet());
  }
