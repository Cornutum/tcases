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
  public String getName()
    {
    return name_;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getLocation())
      .append( getName())
      .toString();
    }

  private final String name_;
  }
