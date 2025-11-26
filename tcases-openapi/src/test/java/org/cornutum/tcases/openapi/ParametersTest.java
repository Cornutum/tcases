//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using
 * variations of parameter locations.
 */
public class ParametersTest extends OpenApiTest
  {
  @Test
  public void whenMultipleLocations()
    {
    verifyRequestInputModel( "parameters-0");
    }
  
  @Test
  public void whenMultipleTypes()
    {
    verifyRequestInputModel( "parameters-1");
    }
  }
