//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.ToString;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.hamcrest.Composites;
import org.hamcrest.Matchers;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toSet;

import java.math.BigDecimal;

/**
 * A composite matcher for {@link Schema} objects.
 */
@SuppressWarnings("rawtypes")
public class SchemaMatcher extends BaseCompositeMatcher<Schema>
  {
  /**
   * Creates a new SchemaMatcher instance.
   */
  public SchemaMatcher( Schema expected)
    {
    super( expected);

    expectThat( valueOf( "type", Schema::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "additionalProperties", this::getAdditionalPropertiesBoolean).matches( Matchers::equalTo));
    expectThat( valueOf( "additionalProperties", this::getAdditionalPropertiesSchema).matches( SchemaMatcher::new));
    expectThat( valueOf( "exclusiveMaximum", Schema::getExclusiveMaximum).matches( Matchers::equalTo));
    expectThat( valueOf( "exclusiveMinimum", Schema::getExclusiveMinimum).matches( Matchers::equalTo));
    expectThat( valueOf( "format", Schema::getFormat).matches( Matchers::equalTo));
    expectThat( valueOf( "items", this::getItems).matches( SchemaMatcher::new));
    expectThat( valueOf( "maxItems", Schema::getMaxItems).matches( Matchers::equalTo));
    expectThat( valueOf( "maxLength", Schema::getMaxLength).matches( Matchers::equalTo));
    expectThat( valueOf( "maxProperties", Schema::getMaxProperties).matches( Matchers::equalTo));
    expectThat( valueOf( "maximum", Schema::getMaximum).matches( Matchers::equalTo));
    expectThat( valueOf( "minItems", Schema::getMinItems).matches( Matchers::equalTo));
    expectThat( valueOf( "minLength", Schema::getMinLength).matches( Matchers::equalTo));
    expectThat( valueOf( "minProperties", Schema::getMinProperties).matches( Matchers::equalTo));
    expectThat( valueOf( "minimum", Schema::getMinimum).matches( Matchers::equalTo));
    expectThat( valueOf( "multipleOf", Schema::getMultipleOf).matches( Matchers::equalTo));
    expectThat( valueOf( "not", Schema::getNot).matches( SchemaMatcher::new));
    expectThat( valueOf( "notFormats", this::getNotFormats).matches( Composites::containsMembers));
    expectThat( valueOf( "notMultipleOfs", this::getNotMultipleOfs).matches( Composites::containsMembers));
    expectThat( valueOf( "nullable", Schema::getNullable).matches( Matchers::equalTo));
    expectThat( valueOf( "patterns", this::getPatterns).matches( Composites::containsMembers));
    expectThat( valueOf( "properties", this::getProperties).matches( containsMembersMatching( Property.Matcher::new)));
    expectThat( valueOf( "readOnly", Schema::getReadOnly).matches( Matchers::equalTo));
    expectThat( valueOf( "required", Schema::getRequired).matches( Composites::containsMembers));
    expectThat( valueOf( "uniqueItems", Schema::getUniqueItems).matches( Matchers::equalTo));
    expectThat( valueOf( "writeOnly", Schema::getWriteOnly).matches( Matchers::equalTo));
    expectThat( valueOf( "allOf", this::getAllOf).matches( containsMembersMatching( SchemaMatcher::new)));
    expectThat( valueOf( "anyOf", this::getAnyOf).matches( containsMembersMatching( SchemaMatcher::new)));
    expectThat( valueOf( "oneOf", this::getOneOf).matches( containsMembersMatching( SchemaMatcher::new)));
    }

  /**
   * Returns "allOf" members for the given schema.
   */
  private List<Schema> getAllOf( Schema schema)
    {
    return Optional.ofNullable( asComposedSchema( schema)).map( c -> c.getAllOf()).orElse( null);
    }

  /**
   * Returns "anyOf" members for the given schema.
   */
  private List<Schema> getAnyOf( Schema schema)
    {
    return Optional.ofNullable( asComposedSchema( schema)).map( c -> c.getAnyOf()).orElse( null);
    }

  /**
   * Returns "oneOf" members for the given schema.
   */
  private List<Schema> getOneOf( Schema schema)
    {
    return Optional.ofNullable( asComposedSchema( schema)).map( c -> c.getOneOf()).orElse( null);
    }

  /**
   * Returns the "items" schema for the given schema.
   */
  private Schema getItems( Schema schema)
    {
    return Optional.ofNullable( asArraySchema( schema)).map( c -> c.getItems()).orElse( null);
    }

  /**
   * Returns the composed set of patterns to match when validating the given schema.
   */
  private Iterable<String> getPatterns( Schema schema)
    {
    return SchemaExtensions.getPatterns( schema);
    }

  /**
   * Returns the composed set of formats to not match when validating the given schema.
   */
  private Iterable<String> getNotFormats( Schema schema)
    {
    return SchemaExtensions.getNotFormats( schema);
    }

  /**
   * Returns the composed set of multipleOfs to not match when validating the given schema.
   */
  private Iterable<BigDecimal> getNotMultipleOfs( Schema schema)
    {
    return SchemaExtensions.getNotMultipleOfs( schema);
    }

  /**
   * Returns the property definitions for the given schema.
   */
  @SuppressWarnings("unchecked")
  private Set<Property> getProperties( Schema schema)
    {
    return
      Optional.ofNullable( (Map<String,Schema>) schema.getProperties())
      .map( properties -> properties.entrySet().stream().map( entry -> new Property( entry.getKey(), entry.getValue())).collect( toSet()))
      .orElse( emptySet());
    }

  /**
   * Returns the "additionalProperties" for this schema as a Schema.
   */
  public Schema getAdditionalPropertiesSchema( Schema schema)
    {
    Object additional = schema.getAdditionalProperties();
    return
      additional instanceof Schema
      ? (Schema) additional
      : null;
    }

  /**
   * Returns the "additionalProperties" for this schema as a Boolean.
   */
  public Boolean getAdditionalPropertiesBoolean( Schema schema)
    {
    return
      getAdditionalPropertiesSchema( schema) == null
      ? (Boolean) schema.getAdditionalProperties()
      : null;
    }

  /**
   * If the given schema is a ComposedSchema, returns it. Otherwise, returns null.
   */
  private ComposedSchema asComposedSchema( Schema schema)
    {
    return
      schema instanceof ComposedSchema
      ? (ComposedSchema) schema
      : null;
    }

  /**
   * If the given schema is a ArraySchema, returns it. Otherwise, returns null.
   */
  private ArraySchema asArraySchema( Schema schema)
    {
    return
      schema instanceof ArraySchema
      ? (ArraySchema) schema
      : null;
    }

  /**
   * Represents a property definition.
   */
  public static class Property
    {
    /**
     * Creates a new Property instance.
     */
    public Property( String name, Schema value)
      {
      name_ = name;
      value_ = value;
      }

    /**
     * Returns the property name.
     */
    public String getName()
      {
      return name_;
      }

    /**
     * Returns the property value.
     */
    public Schema getValue()
      {
      return value_;
      }

    public int hashCode()
      {
      return
        getClass().hashCode()
        ^ Objects.hashCode( name_);
      }

    public boolean equals( Object object)
      {
      Property other =
        object != null && object.getClass().equals( getClass())
        ? (Property) object
        : null;

      return
        other != null
        && Objects.equals( other.getName(), getName());
      }

    public String toString()
      {
      return
        ToString.getBuilder( this)
        .append( getName())
        .toString();
      }
    
    /**
     * A composite matcher for Property objects.
     */
    public static class Matcher extends BaseCompositeMatcher<Property>
      {
      /**
       * Creates a new Matcher instance.
       */
      public Matcher( Property expected)
        {
        super( expected);
        expectThat( valueOf( "name", Property::getName).matches( Matchers::equalTo));
        expectThat( valueOf( "value", Property::getValue).matches( SchemaMatcher::new));
        }
      }
    
    private final String name_;
    private final Schema value_;
    }

  }
