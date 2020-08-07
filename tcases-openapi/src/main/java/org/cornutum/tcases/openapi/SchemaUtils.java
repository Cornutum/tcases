//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.SchemaExtensions.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;

import org.apache.commons.lang3.text.WordUtils;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.LinkedHashMap;
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
   * Returns a concise description of the assertions made by the given schema.
   */
  public static String asserts( Schema<?> schema)
    {
    return
      Optional.ofNullable( schema)
      .map( s -> {
        String type = assertsType( s);
        return
          new StringBuilder()
          .append( WordUtils.capitalize( type))
          .append(
            assertsList(
              assertsMap( type, s).entrySet().stream()
              .map( entry -> String.format( "%s=%s", entry.getKey(), entry.getValue()))))
          .toString();
        })
      .orElse( null);
    }

  /**
   * Returns a description of the type of the given schema.
   */
  private static String assertsType( Schema<?> schema)
    {
    return
      schema.getType() == null?
      "any" :

      !"string".equals( schema.getType())?
      schema.getType() :

      "binary".equals( schema.getFormat())?
      "binary" :

      "byte".equals( schema.getFormat())?
      "byte" :

      "string";
    }

  /**
   * Returns a map of assertion descriptions for the given schema.
   */
  private static Map<String,String> assertsMap( String type, Schema<?> schema)
    {
    Map<String,String> asserts = new LinkedHashMap<String,String>();
    if( !(type.equals( "binary") || type.equals( "byte")))
      {
      Optional.ofNullable( schema.getEnum())
        .ifPresent( v -> {
          asserts.put(
            "enum",
            assertsList( v.stream().map( e -> Objects.toString( e, ""))));
          });
      Optional.ofNullable( schema.getAdditionalProperties())
        .ifPresent( v -> {
          asserts.put(
            "additionalProperties",
            Optional.ofNullable( additionalPropertiesSchema( schema))
            .map( SchemaUtils::asserts)
            .orElse( Objects.toString( v, "")));
          });
      Optional.ofNullable( schema.getExclusiveMaximum())
        .ifPresent( v -> asserts.put( "exclusiveMaximum", Objects.toString( v, "")));
      Optional.ofNullable( schema.getExclusiveMinimum())
        .ifPresent( v -> asserts.put( "exclusiveMinimum", Objects.toString( v, "")));
      Optional.ofNullable( schema.getFormat())
        .ifPresent( v -> asserts.put( "format", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMaxLength())
        .ifPresent( v -> asserts.put( "maxLength", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMaxProperties())
        .ifPresent( v -> asserts.put( "maxProperties", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMaximum())
        .ifPresent( v -> asserts.put( "maximum", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMinLength())
        .ifPresent( v -> asserts.put( "minLength", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMinProperties())
        .ifPresent( v -> asserts.put( "minProperties", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMinimum())
        .ifPresent( v -> asserts.put( "minimum", Objects.toString( v, "")));
      Optional.ofNullable( schema.getMultipleOf())
        .ifPresent( v -> asserts.put( "multipleOf", Objects.toString( v, "")));
      Optional.ofNullable( schema.getNullable())
        .ifPresent( v -> asserts.put( "nullable", Objects.toString( v, "")));
      Optional.ofNullable( schema.getProperties())
        .ifPresent( v -> {
          asserts.put(
            "properties",
            assertsList(
              v.entrySet().stream()
              .map( e -> String.format( "%s=%s", e.getKey(), asserts( e.getValue())))));
          });
      Optional.ofNullable( schema.getRequired())
        .ifPresent( v -> {
          asserts.put(
            "required",
            assertsList( v.stream().map( e -> Objects.toString( e, ""))));
          });
      Optional.ofNullable( schema.getReadOnly())
        .ifPresent( v -> asserts.put( "readOnly", Objects.toString( v, "")));
      Optional.ofNullable( schema.getWriteOnly())
        .ifPresent( v -> asserts.put( "writeOnly", Objects.toString( v, "")));

      Optional.ofNullable( schema.getExtensions())
        .ifPresent( v -> {
          v.entrySet().stream().forEach( e -> asserts.put( e.getKey(), Objects.toString( e.getValue(), "")));
          });

      Optional.ofNullable( asArraySchema( schema))
        .ifPresent( array -> {
          Optional.ofNullable( array.getItems())
            .ifPresent( v -> asserts.put( "items", asserts( v)));
          Optional.ofNullable( array.getMaxItems())
            .ifPresent( v -> asserts.put( "maxItems", Objects.toString( v, "")));
          Optional.ofNullable( array.getMinItems())
            .ifPresent( v -> asserts.put( "minItems", Objects.toString( v, "")));
          Optional.ofNullable( array.getUniqueItems())
            .ifPresent( v -> asserts.put( "uniqueItems", Objects.toString( v, "")));
          });

      Optional.ofNullable( schema.getNot())
        .ifPresent( not -> asserts.put( "not", asserts( not)));

      Optional.ofNullable( asComposedSchema( schema))
        .ifPresent( composed -> {
          Optional.ofNullable( composed.getAllOf())
            .ifPresent( v -> {
              Optional.of( v)
                .filter( schemas -> !schemas.isEmpty())
                .ifPresent( schemas -> asserts.put( "allOf", assertsList( schemas.stream().map( SchemaUtils::asserts))));
              });
          Optional.ofNullable( composed.getAnyOf())
            .ifPresent( v -> {
              Optional.of( v)
                .filter( schemas -> !schemas.isEmpty())
                .ifPresent( schemas -> asserts.put( "anyOf", assertsList( schemas.stream().map( SchemaUtils::asserts))));
              });
          Optional.ofNullable( composed.getOneOf())
            .ifPresent( v -> {
              Optional.of( v)
                .filter( schemas -> !schemas.isEmpty())
                .ifPresent( schemas -> asserts.put( "oneOf", assertsList( schemas.stream().map( SchemaUtils::asserts))));
              });
          });
      }
    
    return asserts;
    }

  /**
   * Returns a description of the given list of assertions.
   */
  public static String assertsList( Stream<String> assertions)
    {
    return
      new StringBuilder()
      .append( '[')
      .append( assertions.collect( joining( ", ")))
      .append( ']')
      .toString();
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
   * If the given schema asserts a schema for additional properties, returns
   * the additional properties schema. Otherwise, returns null.
   */
  public static Schema<?> additionalPropertiesSchema( Schema<?> schema)
    {
    return
      schema.getAdditionalProperties() instanceof Schema
      ? (Schema<?>) schema.getAdditionalProperties()
      : null;
    }

  /**
   * Returns the given schema designated as an object property schema.
   */
  public static Schema<?> toPropertySchema( Schema<?> schema)
    {
    setPropertySchema( schema, true);
    return schema;
    }

  /**
   * Returns true if this is a basic schema without any boolean combinations of subschemas.
   */
  public static boolean isLeafSchema( Schema<?> schema)
    {
    return asComposedSchema( schema) == null && schema.getNot() == null;
    }

  /**
   * Returns true if this schema asserts some any boolean combinations of subschemas.
   */
  public static boolean isNotLeafSchema( Schema<?> schema)
    {
    return !isLeafSchema( schema);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
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
        throw inconsistentAssertions( "type: %s", additionalType, baseType);
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
  private static Schema<?> combineObjectSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineGenericSchemas( context, base, additional);

    // Combine additionalProperties
    Schema<?> baseExtraSchema = additionalPropertiesSchema( base);
    Schema<?> additionalExtraSchema = additionalPropertiesSchema( additional);
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
      .collect( toOrderedSet())
      .stream().collect( toList());
    combined.setRequired( combinedRequired);

    // Combine not required
    setNotRequired( combined, getNotRequired( base));
    addNotRequired( combined, getNotRequired( additional));

    Optional.ofNullable( getNotRequired( combined))
      .flatMap( nr -> combinedRequired.stream().filter( p -> nr.contains( p)).findFirst())
      .ifPresent( p -> {
        throw inconsistentNotAssertion( "required: [%s]", p);
        });
    
    // Combine properties
    Map<String,Schema> basePropertyDefs = Optional.ofNullable( base.getProperties()).orElse( emptyMap());
    Map<String,Schema> additionalPropertyDefs = Optional.ofNullable( additional.getProperties()).orElse( emptyMap());
    Map<String,Schema> combinedPropertyDefs =
      context.resultFor(
        "properties",
        () -> 
        Stream.concat( basePropertyDefs.keySet().stream(), additionalPropertyDefs.keySet().stream())
        .collect( toOrderedSet())
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
  private static Schema<?> combineStringSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
    {
    Schema combined = combineGenericSchemas( context, base, additional);

    // Combine format
    if( base.getFormat() != null && additional.getFormat() != null && !base.getFormat().equals( additional.getFormat()))
      {
      throw inconsistentAssertions( "format: %s", additional.getFormat(), base.getFormat());
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

    Optional.ofNullable( getPatterns( combined))
      .flatMap( ps -> Optional.ofNullable( getNotPatterns( combined)).flatMap( nps -> ps.stream().filter( p -> nps.contains( p)).findFirst()))
      .ifPresent( p -> {
        throw inconsistentNotAssertion( "pattern: '%s'", p);
        });
      
    return combined;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings("rawtypes")
  private static Schema<?> combineIntegerSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
  private static Schema<?> combineBooleanSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
    {
    return combineGenericSchemas( context, base, additional);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  private static Schema<?> combineArraySchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
  private static Schema<?> combineNumberSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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
  private static Schema<?> combineNumericSchemas( OpenApiContext context, Schema<?> base, Schema<?> additional)
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

    Optional.ofNullable( combined.getMultipleOf())
      .flatMap( m -> Optional.ofNullable( getNotMultipleOfs( combined)).flatMap( nms -> nms.stream().filter( nm -> isMultipleOf( m, nm)).findFirst()))
      .ifPresent( nm -> {
        throw inconsistentNotAssertion( "multipleOf: %s", combined.getMultipleOf(), nm);
        });

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

    if( !isMultipleOf( max, min))
      {
      throw inconsistentAssertions( "multipleOf: %s", additional, base);
      }

    return max;
    }

  /**
   * Returns true if the given value is a multiple of the given factor.
   */
  public static boolean isMultipleOf( BigDecimal value, BigDecimal factor)
    {
    return
      value.compareTo( BigDecimal.ZERO) == 0
      ||
      value.remainder( factor).compareTo( BigDecimal.ZERO) == 0;
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes" })
  private static Schema combineGenericSchemas( OpenApiContext context, Schema base, Schema additional)
    {
    return combineGenericSchemas( context, emptySchema(), base, additional);
    }

  /**
   * Returns a new schema that validates any instance that satisfies both the base schema and the additional schema.
   * Throws an exception if a consistent combination is not possible.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  private static <T extends Schema> T combineGenericSchemas( OpenApiContext context, T combined, Schema base, Schema additional)
    {
    // Combine type
    String baseType = base.getType();
    String additionalType = additional.getType();

    if( baseType == null)
      {
      Optional.ofNullable( getNotTypes( base))
        .filter( notTypes -> notTypes.contains( additionalType))
        .ifPresent( notTypes -> {
          throw inconsistentNotAssertion( "type: %s", additionalType);
          });
      }
    if( additionalType == null)
      {
      Optional.ofNullable( getNotTypes( additional))
        .filter( notTypes -> notTypes.contains( baseType))
        .ifPresent( notTypes -> {
          throw inconsistentNotAssertion( "type: %s", baseType);
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
          Set<Object> baseEnums = ((List<Object>)enums).stream().collect( toOrderedSet());
          baseEnums.retainAll( additional.getEnum());
          return baseEnums;
        })
      .filter( enums -> !enums.isEmpty())
      .map( enums -> enums.stream().collect( toList()))
      .orElseThrow( () -> inconsistentAssertions( "enum: %s", additional.getEnum(), base.getEnum())));

    // Combine not enums
    setNotEnums( combined, getNotEnums( base));
    addNotEnums( combined, getNotEnums( additional));

    Optional.ofNullable( combined.getEnum())
      .flatMap( es -> Optional.ofNullable( getNotEnums( combined)).flatMap( nes -> es.stream().filter( e -> nes.contains( e)).findFirst()))
      .ifPresent( e -> {
        throw inconsistentNotAssertion( "enum: %s", e);
        });
    
    // Combine nullable
    combined.setNullable(
      base.getNullable() == null?
      additional.getNullable() :

      additional.getNullable() == null?
      base.getNullable() :

      combineAssertions( "nullable: %s", base.getNullable(), additional.getNullable()));


    // Combine readOnly
    setPropertySchema( combined, isPropertySchema( base) || isPropertySchema( additional));
    if( isPropertySchema( combined))
      {
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
        throw inconsistentAssertions( "%s: true", additionalProp, baseProp);
        }
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
      throw inconsistentAssertions( assertionFormat, additional, base);
      }

    return base;
    }

  /**
   * Returns an exception reporting inconsistent assertions.
   */
  private static <T> IllegalStateException inconsistentAssertions( String assertFormat, T value, String otherFormat, T otherValue)
    {
    return
      new IllegalStateException(
        String.format(
          "Can't combine schema requiring {" + assertFormat + "} with schema requiring {" + otherFormat + "}",
          value,
          otherValue));
    }

  /**
   * Returns an exception reporting inconsistent assertions.
   */
  private static <T> IllegalStateException inconsistentAssertions( String assertFormat, T value, T otherValue)
    {
    return inconsistentAssertions( assertFormat, value, assertFormat, otherValue);
    }

  /**
   * Returns an exception reporting inconsistent assertions.
   */
  private static <T> IllegalStateException inconsistentNotAssertion( String assertFormat, T value, T otherValue)
    {
    return inconsistentAssertions( assertFormat, value, String.format( "not: {%s}", assertFormat), otherValue);
    }

  /**
   * Returns an exception reporting inconsistent assertions.
   */
  private static <T> IllegalStateException inconsistentNotAssertion( String assertFormat, T value)
    {
    return inconsistentNotAssertion( assertFormat, value, value);
    }

  /**
   * Returns a new copy of the given schema.
   */
  public static Schema<?> copySchema( Schema<?> schema)
    {
    return combineSchemas( DEFAULT_CONTEXT, schema, EMPTY_SCHEMA);
    }

  /**
   * Returns true if the given schemas are equal, comparing values for only the specified extensions.
   */
  public static boolean equalsExtended( Schema<?> schema1, Schema<?> schema2, String... extensions)
    {
    Schema<?> compare1 = withExtensions( copySchema( schema1), extensions);
    Schema<?> compare2 = withExtensions( copySchema( schema2), extensions);
    return compare1.equals( compare2);
    }

  /**
   * Returns the given schema after removing all but the given extensions.
   */
  private static Schema<?> withExtensions( Schema<?> schema, String... extensions)
    {
    Map<String,Object> extAll = schema.getExtensions();
    if( extAll!= null)
      {
      schema.setExtensions( null);
      for( String ext : extensions)
        {
        if( extAll.containsKey( ext))
          {
          schema.addExtension( ext, extAll.get( ext));
          }
        }
      }

    return schema;
    }

  /**
   * Returns a new empty schema.
   */
  public static Schema<?> emptySchema()
    {
    return new Schema<Object>();
    }

  /**
   * Returns true if the given schema contains no assertions.
   */
  public static boolean isEmpty( Schema<?> schema)
    {
    return Optional.ofNullable( schema).map( s -> s.equals( EMPTY_SCHEMA)).orElse( true);
    }

  /**
   * Returns true if the given schema is the {@link #FALSE_SCHEMA false schema}.
   */
  public static boolean isFalse( Schema<?> schema)
    {
    return equalsExtended( schema, FALSE_SCHEMA, EXT_NOT_TYPES);
    }

  /**
   * Returns a new schema that will invalidate any instance.
   */
  private static Schema<?> falseSchema()
    {
    Schema<?> falsifier = emptySchema();
    setNotTypes( falsifier, SCHEMA_TYPES);
    falsifier.setNullable( false);

    return falsifier;
    }

  public static final Set<String> SCHEMA_TYPES =
    Arrays.asList( "array", "boolean", "integer", "number", "object", "string")
    .stream().collect( toSet());

  /**
   * A schema that contains no assertions.
   */
  public static final Schema<?> EMPTY_SCHEMA = emptySchema();

  /**
   * A schema that will invalidate any instance.
   */
  public static final Schema<?> FALSE_SCHEMA = falseSchema();

  private static final OpenApiContext DEFAULT_CONTEXT = new OpenApiContext();
  }
