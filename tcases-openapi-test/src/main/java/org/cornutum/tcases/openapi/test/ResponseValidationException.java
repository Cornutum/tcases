//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

/**
 * Reports a failure when validating API request responses.
 */
public class ResponseValidationException extends RuntimeException
  {
  /**
   * Creates a new ResponseValidationException instance.
   */
  public ResponseValidationException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new ResponseValidationException instance.
   */
  public ResponseValidationException( String reason, Throwable cause)
    {
    super( reason, cause);
    }

  private static final long serialVersionUID = 5626606510307898664L;
  }
