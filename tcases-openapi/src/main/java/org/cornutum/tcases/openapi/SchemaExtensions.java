//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import io.swagger.v3.oas.models.media.Schema;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import static java.util.Collections.emptyMap;

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
   * Returns true if the specified schema has a value for the given extension key.
   */
  private static boolean hasExtension( Schema<?> schema, String key)
    {
    return getExtensions( schema).containsKey( key);
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
  }
