//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver.io;

import org.cornutum.tcases.openapi.ExecutionContext;
import org.cornutum.tcases.openapi.resolver.RequestCaseException;

/**
 * Provides a context for exceptions that occur when processing {@link org.cornutum.tcases.openapi.resolver.RequestCase} definitions.
 */
public class RequestCaseContext extends ExecutionContext<RequestCaseException>
  {
  /**
   * Returns an exception to throw for the given failure.
   */
  protected RequestCaseException whenFailure( Exception e)
    {
    return new RequestCaseException( getLocation(), e);
    }
  }
