//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ParamDef.Location;

/**
 * Describes an HTTP authentication input.
 */
public abstract class HttpAuthDef extends AuthDef
  {
  /**
   * Creates a new HttpAuthDef instance.
   */
  protected HttpAuthDef()
    {
    super( Location.HEADER);
    }

  /**
   * Returns the name of this authentication input.
   */
  public String getName()
    {
    return "Authorization";
    }
  }
