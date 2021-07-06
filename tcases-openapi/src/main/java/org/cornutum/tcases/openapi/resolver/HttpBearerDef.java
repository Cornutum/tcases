//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Describes an HTTP Bearer authentication input.
 */
public class HttpBearerDef extends HttpAuthDef
  {
  /**
   * Creates a new HttpBearerDef instance.
   */
  public HttpBearerDef()
    {
    super();
    }
  
  /**
   * Implements the Visitor pattern for this authentication input.
   */
  public void accept( AuthDefVisitor visitor)
    {
    visitor.visit( this);
    }

  public int hashCode()
    {
    return getClass().hashCode();
    }

  public boolean equals( Object object)
    {
    return object != null && object.getClass().equals( getClass());
    }
  }
