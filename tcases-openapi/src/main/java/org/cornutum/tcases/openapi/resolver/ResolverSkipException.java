//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Reports a signal to skip conversion of a {@link RequestCaseDef} into a {@link RequestCase}.
 */
public class ResolverSkipException extends ResolverException
  {  
  private static final long serialVersionUID = -9010510255144888115L;

  /**
   * Creates a new ResolverSkipException instance.
   */
  public ResolverSkipException( String[] location, String reason)
    {
    super( reason);
    location_ = location;
    }

  /**
   * Returns the location of the failure.
   */
  public String[] getLocation()
    {
    return location_;
    }

  private final String[] location_;
  }
