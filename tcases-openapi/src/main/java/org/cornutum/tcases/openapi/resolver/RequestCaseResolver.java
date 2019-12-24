//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Random;

/**
 * Resolves an {@link RequestCaseDef abstract test case} for an API request by producing a {@link RequestCase}
 * that describes an executable instance of this test case.
 */
public class RequestCaseResolver
  {
  /**
   * Creates a new RequestCaseResolver instance.
   */
  public RequestCaseResolver( Random random)
    {
    random_ = random;
    }

  /**
   * Resolves an {@link RequestCaseDef abstract test case} for an API request by producing a {@link RequestCase}
   * that describes an executable instance of this test case.
   */
  public RequestCase resolve( RequestCaseDef requestCaseDef)
    {
    return null;
    }

  private final Random random_;
  }
