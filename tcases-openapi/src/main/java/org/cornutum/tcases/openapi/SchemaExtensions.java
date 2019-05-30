//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import io.swagger.v3.oas.models.media.Schema;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
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
   * Changes the instance types that can be validated by the given schema. 
   */
  public static void setValidTypes( Schema<?> schema, Set<String> validTypes)
    {
    setExtension( schema, EXT_VALID_TYPES, validTypes);
    }

  /**
   * Returns the composed set of patterns to (not) match when validating the given "string" schema.
   */
  public static Set<String> getPatterns( Schema<?> schema)
    {
    return
      hasExtension( schema, EXT_PATTERNS)
      ? getExtension( schema, EXT_PATTERNS)
      : Optional.ofNullable( schema.getPattern()).map( Collections::singleton).orElse( emptySet());
    }

  /**
   * Changes the composed set of patterns to (not) match when validating the given "string" schema.
   */
  public static void setPatterns( Schema<?> schema, Iterable<String> patterns)
    {
    removeExtension( schema, EXT_PATTERNS);
    schema.setPattern( null);

    if( patterns != null)
      {
      for( String pattern : patterns)
        {
        addPattern( schema, pattern);
        }
      }
    }

  /**
   * Changes the composed set of patterns to (not) match when validating the given "string" schema.
   */
  public static void setPatterns( Schema<?> schema, String... patterns)
    {
    setPatterns( schema, Arrays.asList( patterns));
    }

  /**
   * Add to the composed set of patterns to (not) match when validating the given "string" schema.
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
   * Returns the composed set of formats to (not) match when validating the given schema.
   */
  public static Set<String> getFormats( Schema<?> schema)
    {
    return
      hasExtension( schema, EXT_FORMATS)
      ? getExtension( schema, EXT_FORMATS)
      : Optional.ofNullable( schema.getFormat()).map( Collections::singleton).orElse( emptySet());
    }

  /**
   * Changes the composed set of formats to (not) match when validating the given schema.
   */
  public static void setFormats( Schema<?> schema, Iterable<String> formats)
    {
    removeExtension( schema, EXT_FORMATS);
    schema.setFormat( null);

    if( formats != null)
      {
      for( String format : formats)
        {
        addFormat( schema, format);
        }
      }
    }

  /**
   * Changes the composed set of formats to (not) match when validating the given schema.
   */
  public static void setFormats( Schema<?> schema, String... formats)
    {
    setFormats( schema, Arrays.asList( formats));
    }

  /**
   * Add to the composed set of formats to (not) match when validating the given schema.
   */
  public static void addFormat( Schema<?> schema, String format)
    {
    if( format != null)
      {
      if( schema.getFormat() == null)
        {
        schema.setFormat( format);
        }

      Set<String> formats = getExtension( schema, EXT_FORMATS);
      if( formats == null)
        {
        formats = new LinkedHashSet<String>();
        setExtension( schema, EXT_FORMATS, formats);
        }
      formats.add( format);
      }
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

  private static final String EXT_VALID_TYPES = "x-tcases-valid-types";
  private static final String EXT_PATTERNS = "x-tcases-patterns";
  private static final String EXT_FORMATS = "x-tcases-formats";
  }
