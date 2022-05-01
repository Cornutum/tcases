//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for OpenAPI definitions that specify content encodings.
 */
public class EncodingsTest extends OpenApiTest
  {
  @Test
  public void whenApplicationWwwUrlencoded()
    {
    verifyRequestInputModel( "encodings-urlencoded");
    }
  
  @Test
  public void whenMultipartFormData()
    {
    verifyRequestInputModel( "encodings-multipart");
    }
  }
