//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.Objects;
import java.util.Optional;

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
    this( reason, null);
    }
  
  /**
   * Creates a new ResponseValidationException instance.
   */
  public ResponseValidationException( String reason, Throwable cause)
    {
    this( null, null, null, null, reason, cause);
    }
  
  /**
   * Creates a new ResponseValidationException instance.
   */
  public ResponseValidationException( String op, String path, String reason)
    {
    this( op, path, null, null, reason);
    }
  
  /**
   * Creates a new ResponseValidationException instance.
   */
  public ResponseValidationException( String op, String path, Integer statusCode, String location, String reason)
    {
    this( op, path, statusCode, location, reason, null);
    }
  
  /**
   * Creates a new ResponseValidationException instance.
   */
  public ResponseValidationException( String op, String path, Integer statusCode, String location, String reason, Throwable cause)
    {
    super( messageFor( op, path, statusCode, location, reason), cause);
    op_ = op;
    path_ = path;
    statusCode_ = statusCode;
    location_ = location;
    }

  /**
   * Returns the operation of the API request returning an invalid response.
   */
  public String getOperation()
    {
    return op_;
    }

  /**
   * Returns the path of the API request returning an invalid response.
   */
  public String getPath()
    {
    return path_;
    }

  /**
   * Returns the location of the failure in the invalid response.
   */
  public String getLocation()
    {
    return location_;
    }

  /**
   * Returns the status code of the API request returning an invalid response.
   */
  public Integer getStatusCode()
    {
    return statusCode_;
    }

  /**
   * Returns a message for an invalid API response.
   */
  public static String messageFor( String op, String path, Integer statusCode, String location, String message)
    {
    String prefix = 
      String.format(
        "%s%s%s%s",
        Objects.toString( op, ""),
        Optional.ofNullable( path).map( p -> String.format( " %s", p)).orElse( ""),
        Optional.ofNullable( statusCode).map( c -> String.format( " (%s)", c)).orElse( ""),
        Optional.ofNullable( location).map( l -> String.format( ", %s", l)).orElse( ""));

    return
      String.format(
        "%s%s",
        Optional.of( prefix).map( p -> String.format( "%s: ", p)).orElse( ""),
        message);
    }
  
  private final String op_;
  private final String path_;
  private final Integer statusCode_;
  private final String location_;

  private static final long serialVersionUID = 5626606510307898664L;
  }
