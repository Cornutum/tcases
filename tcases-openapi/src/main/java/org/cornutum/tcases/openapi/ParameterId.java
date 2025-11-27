//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import io.swagger.v3.oas.models.parameters.Parameter;

import java.util.Objects;

/**
 * Represents a unique id for a request parameter.
 */
public class ParameterId
  {
  /**
   * Creates a new ParameterId instance.
   */
  public static ParameterId of( Parameter parameter)
    {
    return new ParameterId( parameter.getName(), parameter.getIn());
    }
  
  /**
   * Creates a new ParameterId instance.
   */
  private ParameterId( String name, String location)
    {
    name_ = name;
    location_ = location;
    }

  @Override
  public boolean equals( Object object)
    {
    ParameterId other =
      object instanceof ParameterId
      ? (ParameterId) object
      : null;

    return
      other != null
      && Objects.equals( other.name_, name_)
      && Objects.equals( other.location_, location_);
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( name_)
      ^ Objects.hashCode( location_);
    }
  
  @Override
  public String toString()
    {
    return stringValue( name_, location_);
    }

  /**
   * Returns a parameter id string.
   */
  public static String stringValue( String name, String location)
    {
    return String.format( "%s_L%s_", name, location);    
    }

  private final String name_;
  private final String location_;
  }
