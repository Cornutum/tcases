//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests to handle various media type formats.
 */
public class MediaTypesTest extends OpenApiTest
  {
  @Test
  public void withRequestBody()
    {
    verifyRequestInputModel( "media-types");
    }
  }
