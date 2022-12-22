//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.ExecutionNotifier;

/**
 * Provides a context for exceptions that occur when processing an OpenAPI definition
 */
public class OpenApiContext extends ExecutionNotifier<OpenApiException>
  {
  /**
   * Returns an exception to throw for the given failure.
   */
  @Override
  protected OpenApiException whenFailure( Throwable e)
    {
    return
      OpenApiException.class.isAssignableFrom( e.getClass())
      ? (OpenApiException) e
      : new OpenApiException( getLocation(), e);
    }
  }
