//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.media.Schema;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
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
   * Returns the instance types required to validate the given schema.
   */
  public static Set<String> getRequiredTypes( Schema<?> schema)
    {
    return
      Optional.ofNullable( schema.getType())

      .map( type -> {
        return
          Stream.concat(
            Stream.of( type),

            Optional.ofNullable( getNotTypes( schema))
            .map( notTypes -> notTypes.stream())
            .orElse( Stream.empty()))

          .collect( toOrderedSet());
        })

      .orElse( null);
    }

  /**
   * Changes the instance types that can be validated by the given schema. 
   */
  public static void setValidTypes( Schema<?> schema, Set<String> validTypes)
    {
    setExtension( schema, EXT_VALID_TYPES, validTypes);
    }

  /**
   * Returns if the input model for this schema required type checks
   */
  public static boolean isTypeChecked( Schema<?> schema)
    {
    Boolean checked = getExtension( schema, EXT_TYPE_CHECKED);
    return Optional.ofNullable( checked).orElse( true);
    }

  /**
   * Changes if the input model for this schema required type checks
   */
  public static void setTypeChecked( Schema<?> schema, boolean checked)
    {
    setExtension( schema, EXT_TYPE_CHECKED, checked);
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
  private static final String EXT_NOT_ENUMS = "x-tcases-not-enums";
  private static final String EXT_NOT_MULTIPLEOFS = "x-tcases-not-multipleOfs"; 
  private static final String EXT_NOT_PATTERNS = "x-tcases-not-patterns";
  private static final String EXT_NOT_REQUIRED = "x-tcases-not-required";
  private static final String EXT_NOT_TYPES = "x-tcases-not-types";
  private static final String EXT_PATTERNS = "x-tcases-patterns";
  private static final String EXT_TYPE_CHECKED = "x-tcases-type-checked";
  private static final String EXT_VALID_TYPES = "x-tcases-valid-types";
  }
