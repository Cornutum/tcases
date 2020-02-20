//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

/**
 * Builds {@link Schema} instances.
 *
 */
public class SchemaBuilder
  {
  /**
   * Creates a new builder for a Schema with the given type.
   */
  public static SchemaBuilder ofType( String type)
    {
    return new SchemaBuilder( createSchema( type));
    }

  /**
   * Returns a new schema of the given type.
   */
  private static Schema<?> createSchema( String type)
    {
    Schema<?> schema =
      "array".equals( type)
      ? new ArraySchema()
      : new Schema<Object>();

    schema.setType( type);

    return schema;
    }

  /**
   * Creates a new builder for an empty Schema with undefined type.
   */
  public static SchemaBuilder empty()
    {
    return ofType( null);
    }
  
  /**
   * Creates a new builder for a ComposedSchema with undefined type.
   */
  public static SchemaBuilder composed()
    {
    return composed( null);
    }
  
  /**
   * Creates a new builder for a ComposedSchema with the given type.
   */
  public static SchemaBuilder composed( String type)
    {
    ComposedSchema schema = new ComposedSchema();
    schema.setType( type);
    return new SchemaBuilder( schema);
    }
  
  /**
   * Creates a new builder for an object property Schema with the given type.
   */
  public static SchemaBuilder propertySchema( String type)
    {
    return new SchemaBuilder( SchemaUtils.toPropertySchema( createSchema( type)));
    }

  /**
   * Creates a new SchemaBuilder object.
   */
  private SchemaBuilder( Schema<?> schema)
    {
    schema_ = schema;
    }

  /**
   * Returns the current Schema.
   */
  public Schema<?> build()
    {
    return schema_;
    }

  /**
   * Returns the current ComposedSchema.
   */
  public ComposedSchema compose()
    {
    return asComposed();
    }

  public SchemaBuilder additionalProperties( Boolean additionalProperties)
    {
    schema_.setAdditionalProperties( additionalProperties);
    return this;
    }

  public SchemaBuilder additionalProperties( Schema<?> additionalProperties)
    {
    schema_.setAdditionalProperties( additionalProperties);
    return this;
    }
  
  public SchemaBuilder allOf( Schema<?>... members)
    {
    return allOf( Arrays.asList( members));
    }
  
  @SuppressWarnings("rawtypes")
  public SchemaBuilder allOf( List<Schema> members)
    {
    asComposed().setAllOf( members);
    return this;
    }
  
  public SchemaBuilder anyOf( Schema<?>... members)
    {
    return anyOf( Arrays.asList( members));
    }
  
  @SuppressWarnings("rawtypes")
  public SchemaBuilder anyOf( List<Schema> members)
    {
    asComposed().setAnyOf( members);
    return this;
    }
  
  public SchemaBuilder oneOf( Schema<?>... members)
    {
    return oneOf( Arrays.asList( members));
    }
  
  @SuppressWarnings("rawtypes")
  public SchemaBuilder oneOf( List<Schema> members)
    {
    asComposed().setOneOf( members);
    return this;
    }
  
  @SuppressWarnings("unchecked")
  public SchemaBuilder enums( List<Object> enums)
    {
    ((Schema<Object>) schema_).setEnum( enums);
    return this;
    }
  
  public SchemaBuilder enums( Object... enums)
    {
    return enums( Arrays.asList( enums));
    }
  
  public SchemaBuilder enumNumbers( double... enums)
    {
    return enums( Arrays.stream( enums).mapToObj( d -> new BigDecimal( String.valueOf( d))).collect( toList()));
    }
  
  public SchemaBuilder exclusiveMaximum( Boolean exclusiveMaximum)
    {
    schema_.setExclusiveMaximum( exclusiveMaximum);
    return this;
    }
  
  public SchemaBuilder exclusiveMinimum( Boolean exclusiveMinimum)
    {
    schema_.setExclusiveMinimum( exclusiveMinimum);
    return this;
    }
  
  public SchemaBuilder format( String format)
    {
    schema_.setFormat( format);
    return this;
    }
  
  public SchemaBuilder maxItems( Integer maxItems)
    {
    schema_.setMaxItems( maxItems);
    return this;
    }
  
  public SchemaBuilder items( Schema<?> items)
    {
    asArraySchema().setItems( items);
    return this;
    }
  
  public SchemaBuilder maxLength( Integer maxLength)
    {
    schema_.setMaxLength( maxLength);
    return this;
    }
  
  public SchemaBuilder maxProperties( Integer maxProperties)
    {
    schema_.setMaxProperties( maxProperties);
    return this;
    }
  
  public SchemaBuilder maximum( BigDecimal maximum)
    {
    schema_.setMaximum( maximum);
    return this;
    }
  
  public SchemaBuilder maximum( int maximum)
    {
    return maximum( new BigDecimal( maximum));
    }
  
  public SchemaBuilder maximum( double maximum)
    {
    return maximum( new BigDecimal( String.valueOf( maximum)));
    }
  
  public SchemaBuilder minItems( Integer minItems)
    {
    schema_.setMinItems( minItems);
    return this;
    }
  
  public SchemaBuilder minLength( Integer minLength)
    {
    schema_.setMinLength( minLength);
    return this;
    }
  
  public SchemaBuilder minProperties( Integer minProperties)
    {
    schema_.setMinProperties( minProperties);
    return this;
    }
  
  public SchemaBuilder minimum( BigDecimal minimum)
    {
    schema_.setMinimum( minimum);
    return this;
    }
  
  public SchemaBuilder minimum( int minimum)
    {
    return minimum( new BigDecimal( minimum));
    }
  
  public SchemaBuilder minimum( double minimum)
    {
    return minimum( new BigDecimal( String.valueOf( minimum)));
    }
  
  public SchemaBuilder multipleOf( BigDecimal multipleOf)
    {
    schema_.setMultipleOf( multipleOf);
    return this;
    }
  
  public SchemaBuilder multipleOf( int multipleOf)
    {
    return multipleOf( new BigDecimal( multipleOf));
    }
  
  public SchemaBuilder multipleOf( double multipleOf)
    {
    return multipleOf( new BigDecimal( String.valueOf( multipleOf)));
    }
  
  public SchemaBuilder notEnums( List<Object> enums)
    {
    SchemaExtensions.setNotEnums( schema_, enums);
    return this;
    }
  
  public SchemaBuilder notEnums( Object... enums)
    {
    return notEnums( Arrays.asList( enums));
    }
  
  public SchemaBuilder notEnumNumbers( double... enums)
    {
    return notEnums( Arrays.stream( enums).mapToObj( d -> new BigDecimal( String.valueOf( d))).collect( toList()));
    }
  
  public SchemaBuilder notMultipleOfs( Number... multipleOfs)
    {
    SchemaExtensions.setNotMultipleOfs(
      schema_,
      Arrays.stream( multipleOfs)
      .map( number -> new BigDecimal( number.toString()))
      .collect( toSet()));

    return this;
    }
  
  public SchemaBuilder notPatterns( String... patterns)
    {
    SchemaExtensions.setNotPatterns( schema_, patterns);
    return this;
    }
  
  public SchemaBuilder notRequired( String... required)
    {
    SchemaExtensions.setNotRequired( schema_, Arrays.asList( required));
    return this;
    }
  
  public SchemaBuilder notTypes( String... types)
    {
    SchemaExtensions.setNotTypes( schema_, Arrays.asList( types));
    return this;
    }
  
  public SchemaBuilder nullable( Boolean nullable)
    {
    schema_.setNullable( nullable);
    return this;
    }
  
  public SchemaBuilder patterns( String... patterns)
    {
    SchemaExtensions.setPatterns( schema_, patterns);
    return this;
    }
  
  @SuppressWarnings("rawtypes")
  public SchemaBuilder property( String name, Schema<?> schema)
    {
    Map<String,Schema> properties = Optional.ofNullable( schema_.getProperties()).orElse( new HashMap<String,Schema>());
    properties.put( name, schema);
    return properties( properties);
    }
  
  @SuppressWarnings("rawtypes")
  public SchemaBuilder properties( Map<String,Schema> properties)
    {
    schema_.setProperties( properties);
    return this;
    }
  
  public SchemaBuilder readOnly( Boolean readOnly)
    {
    schema_.setReadOnly( readOnly);
    return this;
    }
  
  public SchemaBuilder required( String... required)
    {
    return required( Arrays.asList( required));
    }
  
  public SchemaBuilder required( List<String> required)
    {
    schema_.setRequired( required);
    return this;
    }
  
  public SchemaBuilder uniqueItems( Boolean uniqueItems)
    {
    schema_.setUniqueItems( uniqueItems);
    return this;
    }
  
  public SchemaBuilder writeOnly( Boolean writeOnly)
    {
    schema_.setWriteOnly( writeOnly);
    return this;
    }

  /**
   * Returns this ComposedSchema.
   */
  private ComposedSchema asComposed()
    {
    return
      Optional.of( schema_)
      .filter( schema -> schema instanceof ComposedSchema)
      .map( schema -> (ComposedSchema) schema)
      .orElseThrow( () -> new IllegalStateException( "This is not a ComposedSchema"));
    }

  /**
   * Returns this ArraySchema.
   */
  private ArraySchema asArraySchema()
    {
    return
      Optional.of( schema_)
      .filter( schema -> schema instanceof ArraySchema)
      .map( schema -> (ArraySchema) schema)
      .orElseThrow( () -> new IllegalStateException( "This is not an ArraySchema"));
    }

  private Schema<?> schema_;
  }
