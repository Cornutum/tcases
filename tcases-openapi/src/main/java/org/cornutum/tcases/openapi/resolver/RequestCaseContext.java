//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.ExecutionContext;

/**
 * Provides a context for exceptions that occur when processing {@link org.cornutum.tcases.openapi.resolver.RequestCase} definitions.
 */
public class RequestCaseContext extends ExecutionContext<RequestCaseException>
  {
  /**
   * Returns an exception to throw for the given failure.
   */
  @Override
protected RequestCaseException whenFailure( Throwable e)
    {
    return new RequestCaseException( getLocation(), e);
    }
  }
