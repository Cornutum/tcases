//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.Optional;

/**
 * Represents a schema validation error.
 */
public class SchemaValidationError
  {
  /**
   * Creates a new SchemaValidationError instance.
   */
  public SchemaValidationError( String dataLocation, String schemaLocation, String reason)
    {
    dataLocation_ = dataLocation;
    schemaLocation_ = schemaLocation;
    reason_ = reason;
    }

  /**
   * Returns the data location of the error.
   */
  public String getDataLocation()
    {
    return dataLocation_;
    }

  /**
   * Returns the schema location of the error.
   */
  public String getSchemaLocation()
    {
    return schemaLocation_;
    }

  /**
   * Returns the complete location of the error.
   */
  public String getLocation()
    {
    return
      String.format(
        "%s%s",
        getDataLocation(),
        Optional.of( getSchemaLocation()).filter( path -> !path.isEmpty()).map( path -> String.format( "#%s", path)).orElse( ""));
      }

  /**
   * Returns the reason for the error.
   */
  public String getReason()
    {
    return reason_;
    }

  @Override
  public String toString()
    {
    String location =
      Optional.of( getLocation())
      .filter( loc -> !loc.isEmpty())
      .map( loc -> String.format( "%s: ", loc))
      .orElse( "");
    
    return String.format( "%s%s", location, getReason());
    }
  
  private final String dataLocation_;
  private final String schemaLocation_;
  private final String reason_;
  }
