//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ParamDef.Location;
import org.cornutum.tcases.util.ToString;

import java.util.Objects;

/**
 * Describes an API key authentication input.
 */
public class ApiKeyDef extends AuthDef
  {
  /**
   * Creates a new AuthDef instance.
   */
  public ApiKeyDef( Location location, String name)
    {
    super( location);
    name_ = name;
    }

  /**
   * Returns the name of this authentication input.
   */
  @Override
public String getName()
    {
    return name_;
    }
  
  /**
   * Implements the Visitor pattern for this authentication input.
   */
  @Override
public void accept( AuthDefVisitor visitor)
    {
    visitor.visit( this);
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getLocation())
      .append( getName())
      .toString();
    }

  @Override
public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getLocation())
      ^ Objects.hashCode( getName());
    }

  @Override
public boolean equals( Object object)
    {
    ApiKeyDef other =
      object != null && object.getClass().equals( getClass())
      ? (ApiKeyDef) object
      : null;

    return
      other != null
      && Objects.equals( other.getLocation(), getLocation())
      && Objects.equals( other.getName(), getName());
    }
  
  private final String name_;
  }
