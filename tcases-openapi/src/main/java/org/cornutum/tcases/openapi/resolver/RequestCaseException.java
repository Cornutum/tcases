//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Reports a failure to convert a {@link org.cornutum.tcases.TestCase TestCase} into a {@link RequestCaseDef}.
 */
public class RequestCaseException extends RuntimeException
  {
  private static final long serialVersionUID = 1994400157242603177L;

  /**
   * Creates a new RequestCaseException instance.
   */
  public RequestCaseException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new RequestCaseException instance.
   */
  public RequestCaseException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
