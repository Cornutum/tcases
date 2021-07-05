//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests to handle security requirements.
 */
public class SecurityTest extends OpenApiTest
  {
  @Test
  public void whenApiReqsNone()
    {
    verifyRequestInputModel( "auth-api-none");
    }
  
  @Test
  public void whenApiReqsEmpty()
    {
    verifyRequestInputModel( "auth-api-empty");
    }
  
  @Test
  public void whenApiReqsOne()
    {
    verifyRequestInputModel( "auth-api-one");
    }
  
  @Test
  public void whenApiReqsMany()
    {
    verifyRequestInputModel( "auth-api-many");
    }
  }
