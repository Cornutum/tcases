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
public class ResponseUnvalidatedException extends ResponseValidationException
  {
  /**
   * Creates a new ResponseUnvalidatedException instance.
   */
  public ResponseUnvalidatedException( String op, String path, int statusCode, String reason)
    {
    super( ResponseValidator.messageFor( op, path, statusCode, reason));
    op_ = op;
    path_ = path;
    statusCode_ = statusCode;
    }

  /**
   * Returns the operation of the API request returning an unvalidated response.
   */
  public String getOperation()
    {
    return op_;
    }

  /**
   * Returns the path of the API request returning an unvalidated response.
   */
  public String getPath()
    {
    return path_;
    }

  /**
   * Returns the status code of the API request returning an unvalidated response.
   */
  public int getStatusCode()
    {
    return statusCode_;
    }
  
  private final String op_;
  private final String path_;
  private final int statusCode_;

  private static final long serialVersionUID = -847135141260063875L;
  }
