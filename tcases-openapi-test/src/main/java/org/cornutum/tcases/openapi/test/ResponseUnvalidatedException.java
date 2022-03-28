//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

/**
 * Reports a condition that prevents API request validation.
 */
public class ResponseUnvalidatedException extends ResponseValidationException
  {
  /**
   * Creates a new ResponseUnvalidatedException instance.
   */
  public ResponseUnvalidatedException( String op, String path, Integer statusCode, String location, String reason)
    {
    super( op, path, statusCode, location, reason);
    }
  
  private static final long serialVersionUID = -847135141260063875L;
  }
