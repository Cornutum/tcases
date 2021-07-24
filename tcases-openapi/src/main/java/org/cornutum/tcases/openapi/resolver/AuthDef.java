//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ParamDef.Location;
import org.cornutum.tcases.util.ToString;

/**
 * Describes a request authentication input.
 */
public abstract class AuthDef
  {
  /**
   * Creates a new AuthDef instance.
   */
  protected AuthDef( Location location)
    {
    location_ = location;
    }

  /**
   * Returns the location of this authentication input.
   */
  public Location getLocation()
    {
    return location_;
    }

  /**
   * Returns the name of this authentication input.
   */
  public abstract String getName();

  /**
   * Implements the Visitor pattern for this authentication input.
   */
  public abstract void accept( AuthDefVisitor visitor);

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }

  private final Location location_;
  }
