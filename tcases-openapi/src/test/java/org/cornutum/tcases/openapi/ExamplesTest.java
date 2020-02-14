//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests using the third-party OpenAPI examples.
 */
public class ExamplesTest extends OpenApiTest
  {
  /**
   * Verify request input model for standard OpenAPI pet store example.
   */
  @Test
  public void verifyPetStoreRequests()
    {
    verifyRequestInputModel( "petstore-expanded", "petstore-requests");
    }
  
  /**
   * Verify response input model for standard OpenAPI pet store example.
   */
  @Test
  public void verifyPetStoreResponses()
    {
    verifyResponseInputModel( "petstore-expanded", "petstore-responses");
    }
  
  /**
   * Verify response input model for a <A href="http://niem.github.io/json/sample-schema/">NIEM vehicle</A> example.
   */
  @Test
  public void verifyNiemVehicleResponses()
    {
    verifyResponseInputModel( "niem-vehicle");
    }
  }
