//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import io.swagger.v3.oas.models.media.Schema;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;

/**
 * Defines methods for accessing Tcases extensions to an OpenAPI {@link Schema} object.
 */
public final class SchemaExtensions
  {
  /**
   * Creates a new SchemaExtensions instance.
   */
  private SchemaExtensions()
    {
    // Static methods only
    }

  /**
   * Returns true if the instance types that can be validated are defined for the given schema.
   */
  public static boolean hasValidTypes( Schema<?> schema)
    {
    return hasExtension( schema, EXT_VALID_TYPES);
    }

  /**
   * Returns the instance types that can be validated by the given schema. Returns null if any type can be validated.
   */
  public static Set<String> getValidTypes( Schema<?> schema)
    {
    return getExtension( schema, EXT_VALID_TYPES);
    }

  /**
   * Returns the composed set of types that must not be required when validating the given schema.
   */
  public static Set<String> getNotTypes( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_TYPES);
    }

  /**
   * Changes the composed set of types that must not be required when validating the given schema.
   */
  public static void setNotTypes( Schema<?> schema, Iterable<String> notTypes)
    {
    removeExtension( schema, EXT_NOT_TYPES);
    addNotTypes( schema, notTypes);
    }

  /**
   * Adds to the composed set of types that must not be required when validating the given schema.
   */
  public static void addNotType( Schema<?> schema, String type)
    {
    Set<String> notTypes = getExtension( schema, EXT_NOT_TYPES);
    if( notTypes == null)
      {
      notTypes = new LinkedHashSet<String>();
      setExtension( schema, EXT_NOT_TYPES, notTypes);
      }
    notTypes.add( type);
    }

  /**
   * Adds to the composed set of types that must not be required when validating the given schema.
   */
  public static void addNotTypes( Schema<?> schema, Iterable<String> notTypes)
    {
    if( notTypes != null)
      {
      for( String type : notTypes)
        {
        addNotType( schema, type);
        }
      }
    }

  /**
   * Changes the instance types that can be validated by the given schema. 
   */
  public static void setValidTypes( Schema<?> schema, Set<String> validTypes)
    {
    setExtension( schema, EXT_VALID_TYPES, validTypes);
    }

  /**
   * Returns the disjunctive normal form of this given schema.
   */
  public static Dnf getDnf( Schema<?> schema)
    {
    return getExtension( schema, EXT_DNF);
    }

  /**
   * Changes the disjunctive normal form of this given schema.
   */
  public static void setDnf( Schema<?> schema, Dnf dnf)
    {
    setExtension( schema, EXT_DNF, dnf);
    }

  /**
   * Returns the composed set of patterns to match when validating the given string schema.
   */
  public static Set<String> getPatterns( Schema<?> schema)
    {
    return
      hasExtension( schema, EXT_PATTERNS)
      ? getExtension( schema, EXT_PATTERNS)
      : Optional.ofNullable( schema.getPattern()).map( Collections::singleton).orElse( emptySet());
    }

  /**
   * Changes the composed set of patterns to match when validating the given string schema.
   */
  public static void setPatterns( Schema<?> schema, Iterable<String> patterns)
    {
    removeExtension( schema, EXT_PATTERNS);
    schema.setPattern( null);
    addPatterns( schema, patterns);
    }

  /**
   * Changes the composed set of patterns to match when validating the given string schema.
   */
  public static void setPatterns( Schema<?> schema, String... patterns)
    {
    setPatterns( schema, Arrays.asList( patterns));
    }

  /**
   * Adds to the composed set of patterns to match when validating the given string schema.
   */
  public static void addPatterns( Schema<?> schema, Iterable<String> patterns)
    {
    if( patterns != null)
      {
      for( String pattern : patterns)
        {
        addPattern( schema, pattern);
        }
      }
    }

  /**
   * Adds to the composed set of patterns to match when validating the given string schema.
   */
  public static void addPattern( Schema<?> schema, String pattern)
    {
    if( pattern != null)
      {
      if( schema.getPattern() == null)
        {
        schema.setPattern( pattern);
        }

      Set<String> patterns = getExtension( schema, EXT_PATTERNS);
      if( patterns == null)
        {
        patterns = new LinkedHashSet<String>();
        setExtension( schema, EXT_PATTERNS, patterns);
        }
      patterns.add( pattern);
      }
    }

  /**
   * Returns the composed set of formats to not match when validating the given schema.
   */
  public static Set<String> getNotFormats( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_FORMATS);
    }

  /**
   * Changes the composed set of formats to not match when validating the given schema.
   */
  public static void setNotFormats( Schema<?> schema, Iterable<String> formats)
    {
    removeExtension( schema, EXT_NOT_FORMATS);
    addNotFormats( schema, formats);
    }

  /**
   * Changes the composed set of formats to not match when validating the given schema.
   */
  public static void setNotFormats( Schema<?> schema, String... formats)
    {
    setNotFormats( schema, Arrays.asList( formats));
    }

  /**
   * Adds to the composed set of formats to not match when validating the given schema.
   */
  public static void addNotFormats( Schema<?> schema, Iterable<String> formats)
    {
    if( formats != null)
      {
      for( String format : formats)
        {
        addNotFormat( schema, format);
        }
      }
    }

  /**
   * Adds to the composed set of formats to not match when validating the given schema.
   */
  public static void addNotFormat( Schema<?> schema, String format)
    {
    if( format != null)
      {
      Set<String> formats = getExtension( schema, EXT_NOT_FORMATS);
      if( formats == null)
        {
        formats = new LinkedHashSet<String>();
        setExtension( schema, EXT_NOT_FORMATS, formats);
        }
      formats.add( format);
      }
    }

  /**
   * Returns the composed set of patterns to not match when validating the given schema.
   */
  public static Set<String> getNotPatterns( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_PATTERNS);
    }

  /**
   * Changes the composed set of patterns to not match when validating the given schema.
   */
  public static void setNotPatterns( Schema<?> schema, Iterable<String> patterns)
    {
    removeExtension( schema, EXT_NOT_PATTERNS);
    addNotPatterns( schema, patterns);
    }

  /**
   * Changes the composed set of patterns to not match when validating the given schema.
   */
  public static void setNotPatterns( Schema<?> schema, String... patterns)
    {
    setNotPatterns( schema, Arrays.asList( patterns));
    }

  /**
   * Adds to the composed set of patterns to not match when validating the given schema.
   */
  public static void addNotPatterns( Schema<?> schema, Iterable<String> patterns)
    {
    if( patterns != null)
      {
      for( String pattern : patterns)
        {
        addNotPattern( schema, pattern);
        }
      }
    }

  /**
   * Adds to the composed set of patterns to not match when validating the given schema.
   */
  public static void addNotPattern( Schema<?> schema, String pattern)
    {
    if( pattern != null)
      {
      Set<String> patterns = getExtension( schema, EXT_NOT_PATTERNS);
      if( patterns == null)
        {
        patterns = new LinkedHashSet<String>();
        setExtension( schema, EXT_NOT_PATTERNS, patterns);
        }
      patterns.add( pattern);
      }
    }

  /**
   * Returns the composed set of "not" schemas to use when validating the given schema.
   */
  public static List<Schema<?>> getNots( Schema<?> schema)
    {
    if( !hasExtension( schema, EXT_NOTS))
      {
      setNots( schema, schema.getNot());
      }

    return getExtension( schema, EXT_NOTS);
    }

  /**
   * Changes the composed set of "not" schemas to use when validating the given schema.
   */
  public static void setNots( Schema<?> schema, Iterable<Schema<?>> nots)
    {
    removeExtension( schema, EXT_NOTS);
    schema.setNot( null);
    addNots( schema, nots);
    }

  /**
   * Changes the composed set of "not" schemas to use when validating the given schema.
   */
  public static void setNots( Schema<?> schema, Schema<?>... nots)
    {
    setNots( schema, Arrays.asList( nots));
    }

  /**
   * Adds to the composed set of "not" schemas to use when validating the given schema.
   */
  public static void addNots( Schema<?> schema, Iterable<Schema<?>> nots)
    {
    if( nots != null)
      {
      for( Schema<?> not : nots)
        {
        addNot( schema, not);
        }
      }
    }

  /**
   * Adds to the composed set of "not" schemas to use when validating the given schema.
   */
  public static void addNot( Schema<?> schema, Schema<?> not)
    {
    if( not != null)
      {
      if( schema.getNot() == null)
        {
        schema.setNot( not);
        }

      List<Schema<?>> nots = getExtension( schema, EXT_NOTS);
      if( nots == null)
        {
        nots = new ArrayList<Schema<?>>();
        setExtension( schema, EXT_NOTS, nots);
        }
      nots.add( not);
      }
    }

  /**
   * Returns the composed set of multipleOfs to not match when validating the given numeric schema.
   */
  public static Set<BigDecimal> getNotMultipleOfs( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_MULTIPLEOFS);
    }

  /**
   * Changes the composed set of multipleOfs to not match when validating the given numeric schema.
   */
  public static void setNotMultipleOfs( Schema<?> schema, Iterable<BigDecimal> multipleOfs)
    {
    removeExtension( schema, EXT_NOT_MULTIPLEOFS);
    addNotMultipleOfs( schema, multipleOfs);
    }

  /**
   * Changes the composed set of multipleOfs to not match when validating the given numeric schema.
   */
  public static void setNotMultipleOfs( Schema<?> schema, BigDecimal... multipleOfs)
    {
    setNotMultipleOfs( schema, Arrays.asList( multipleOfs));
    }

  /**
   * Adds to the composed set of multipleOfs to not match when validating the given numeric schema.
   */
  public static void addNotMultipleOf( Schema<?> schema, BigDecimal multipleOf)
    {
    if( multipleOf != null)
      {
      Set<BigDecimal> multipleOfs = getExtension( schema, EXT_NOT_MULTIPLEOFS);
      if( multipleOfs == null)
        {
        multipleOfs = new LinkedHashSet<BigDecimal>();
        setExtension( schema, EXT_NOT_MULTIPLEOFS, multipleOfs);
        }
      multipleOfs.add( multipleOf);
      }
    }

  /**
   * Adds to the composed set of multipleOfs to not match when validating the given numeric schema.
   */
  public static void addNotMultipleOfs( Schema<?> schema, Iterable<BigDecimal> multipleOfs)
    {
    if( multipleOfs != null)
      {
      for( BigDecimal multipleOf : multipleOfs)
        {
        addNotMultipleOf( schema, multipleOf);
        }
      }
    }

  /**
   * Returns the composed set of enums to not match when validating the given schema.
   */
  public static List<Object> getNotEnums( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_ENUMS);
    }

  /**
   * Changes the composed set of enums to not match when validating the given schema.
   */
  public static void setNotEnums( Schema<?> schema, Iterable<?> enums)
    {
    removeExtension( schema, EXT_NOT_ENUMS);
    addNotEnums( schema, enums);
    }

  /**
   * Adds to the composed set of enums to not match when validating the given schema.
   */
  public static void addNotEnum( Schema<?> schema, Object enumValue)
    {
    if( enumValue != null)
      {
      List<Object> enums = getExtension( schema, EXT_NOT_ENUMS);
      if( enums == null)
        {
        enums = new ArrayList<Object>();
        setExtension( schema, EXT_NOT_ENUMS, enums);
        }
      enums.add( enumValue);
      }
    }

  /**
   * Adds to the composed set of enums to not match when validating the given schema.
   */
  public static void addNotEnums( Schema<?> schema, Iterable<?> enums)
    {
    if( enums != null)
      {
      for( Object enumValue : enums)
        {
        addNotEnum( schema, enumValue);
        }
      }
    }

  /**
   * Returns the composed schema for additional properties to not match when validating the given object schema.
   */
  public static Schema<?> getNotAdditionalProperties( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_ADDITIONALPROPERTIES);
    }

  /**
   * Changes the composed schema for additional properties to not match when validating the given object schema.
   */
  public static void setNotAdditionalProperties( Schema<?> schema, Schema<?> additionalProperties)
    {
    setExtension( schema, EXT_NOT_ADDITIONALPROPERTIES, additionalProperties);
    }

  /**
   * Returns the composed set of properties to not match when validating the given object schema.
   */
  @SuppressWarnings("rawtypes")
  public static Map<String,Schema> getNotProperties( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_PROPERTIES);
    }

  /**
   * Changes the composed set of properties to not match when validating the given object schema.
   */
  @SuppressWarnings("rawtypes")
  public static void setNotProperties( Schema<?> schema, Map<String,Schema> properties)
    {
    removeExtension( schema, EXT_NOT_PROPERTIES);
    addNotProperties( schema, properties);
    }

  /**
   * Adds to the composed set of properties to not match when validating the given object schema.
   */
  @SuppressWarnings("rawtypes")
  public static void addNotProperty( Schema<?> schema, String name, Schema value)
    {
    Map<String,Schema> properties = getExtension( schema, EXT_NOT_PROPERTIES);
    if( properties == null)
      {
      properties = new LinkedHashMap<String,Schema>();
      setExtension( schema, EXT_NOT_PROPERTIES, properties);
      }
    properties.put( name, value);
    }

  /**
   * Adds to the composed set of properties to not match when validating the given object schema.
   */
  @SuppressWarnings("rawtypes")
  public static void addNotProperties( Schema<?> schema, Map<String,Schema> properties)
    {
    if( properties != null)
      {
      for( String property : properties.keySet())
        {
        addNotProperty( schema, property, properties.get( property));
        }
      }
    }

  /**
   * Returns the composed set of properties that must not be required when validating the given object schema.
   */
  public static Set<String> getNotRequired( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_REQUIRED);
    }

  /**
   * Changes the composed set of properties that must not be required when validating the given object schema.
   */
  public static void setNotRequired( Schema<?> schema, Iterable<String> notRequired)
    {
    removeExtension( schema, EXT_NOT_REQUIRED);
    addNotRequired( schema, notRequired);
    }

  /**
   * Adds to the composed set of properties that must not be required when validating the given object schema.
   */
  public static void addNotRequired( Schema<?> schema, String property)
    {
    Set<String> notRequired = getExtension( schema, EXT_NOT_REQUIRED);
    if( notRequired == null)
      {
      notRequired = new LinkedHashSet<String>();
      setExtension( schema, EXT_NOT_REQUIRED, notRequired);
      }
    notRequired.add( property);
    }

  /**
   * Adds to the composed set of properties that must not be required when validating the given object schema.
   */
  public static void addNotRequired( Schema<?> schema, Iterable<String> notRequired)
    {
    if( notRequired != null)
      {
      for( String property : notRequired)
        {
        addNotRequired( schema, property);
        }
      }
    }

  /**
   * Returns the composed schema array items to not match when validating the given array schema.
   */
  public static Schema<?> getNotItems( Schema<?> schema)
    {
    return getExtension( schema, EXT_NOT_ITEMS);
    }

  /**
   * Changes the composed schema array items to not match when validating the given array schema.
   */
  public static void setNotItems( Schema<?> schema, Schema<?> items)
    {
    setExtension( schema, EXT_NOT_ITEMS, items);
    }

  /**
   * Returns true if the specified schema has a value for the given extension key.
   */
  private static boolean hasExtension( Schema<?> schema, String key)
    {
    return getExtensions( schema).containsKey( key);
    }

  /**
   * Removes the value of the given extension key for the specified schema.
   */
  private static void removeExtension( Schema<?> schema, String key)
    {
    if( hasExtension( schema, key))
      {
      getExtensions( schema).remove( key);
      }
    }

  /**
   * Returns the value of the given extension key for the specified schema.
   */
  @SuppressWarnings("unchecked")
  private static <T> T getExtension( Schema<?> schema, String key)
    {
    return (T) getExtensions( schema).get( key);
    }

  /**
   * Changes the value of the given extension key for the specified schema.
   */
  private static void setExtension( Schema<?> schema, String key, Object value)
    {
    Map<String,Object> extensions = Optional.ofNullable( schema.getExtensions()).orElse( new HashMap<String,Object>());
    extensions.put( key, value);
    schema.setExtensions( extensions);
    }

  /**
   * Returns the extensions for the given schema
   */
  private static Map<String,Object> getExtensions( Schema<?> schema)
    {
    return Optional.ofNullable( schema.getExtensions()).orElse( emptyMap());
    }

  private static final String EXT_DNF = "x-tcases-dnf";
  private static final String EXT_NOTS = "x-tcases-nots";
  private static final String EXT_NOT_ADDITIONALPROPERTIES = "x-tcases-not-additionalProperties";
  private static final String EXT_NOT_ENUMS = "x-tcases-not-enums";
  private static final String EXT_NOT_FORMATS = "x-tcases-not-formats";
  private static final String EXT_NOT_ITEMS = "x-tcases-not-items";
  private static final String EXT_NOT_MULTIPLEOFS = "x-tcases-not-multipleOfs"; 
  private static final String EXT_NOT_PATTERNS = "x-tcases-not-patterns";
  private static final String EXT_NOT_PROPERTIES = "x-tcases-not-properties"; 
  private static final String EXT_NOT_REQUIRED = "x-tcases-not-required";
  private static final String EXT_NOT_TYPES = "x-tcases-not-types";
  private static final String EXT_PATTERNS = "x-tcases-patterns";
  private static final String EXT_VALID_TYPES = "x-tcases-valid-types";
  }
