//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

/**
 * Reports a failure deriving input models from API examples.
 */
public class ExampleException extends OpenApiException
  {
  private static final long serialVersionUID = 465493256142306159L;

  /**
   * Creates a new ExampleException instance.
   */
  public ExampleException( String[] location, String reason)
    {
    super( reason);
    location_ = location;
    }

  /**
   * Returns the location of the failure
   */
  public String[] getLocation()
    {
    return location_;
    }

  private final String[] location_;
  }
