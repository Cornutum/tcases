//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests using the standard OpenAPI Pet Store example.
 */
public class PetStoreTest extends OpenApiTest
  {
  @Test
  public void verifyPetStoreRequests()
    {
    verifyRequestInputModel( "petstore-expanded", "petstore-requests");
    }
  
  @Test
  public void verifyPetStoreResponses()
    {
    verifyResponseInputModel( "petstore-expanded", "petstore-responses");
    }

  }
