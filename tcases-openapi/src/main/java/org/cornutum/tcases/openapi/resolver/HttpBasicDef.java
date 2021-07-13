//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Describes an HTTP Basic authentication input.
 */
public class HttpBasicDef extends HttpAuthDef
  {
  /**
   * Creates a new HttpBasicDef instance.
   */
  public HttpBasicDef()
    {
    super();
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
public int hashCode()
    {
    return getClass().hashCode();
    }

  @Override
public boolean equals( Object object)
    {
    return object != null && object.getClass().equals( getClass());
    }
  }
