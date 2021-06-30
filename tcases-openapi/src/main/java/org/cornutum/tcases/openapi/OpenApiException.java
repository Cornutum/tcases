//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import org.apache.commons.lang3.StringUtils;

/**
 * Reports an error processing an OpenAPI definition.
 */
public class OpenApiException extends RuntimeException
  {
  private static final long serialVersionUID = 4329815268207761986L;

  /**
   * Creates a new OpenApiException instance.
   */
  public OpenApiException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new OpenApiException instance.
   */
  public OpenApiException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  
  /**
   * Creates a new OpenApiException instance.
   */
  public OpenApiException( String[] location, Throwable cause)
    {
    super( String.format( "Error processing %s", StringUtils.join( location, ", ")), cause);
    }
  }
