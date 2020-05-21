//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.testwriter.encoder.UriEncoder;
import static org.cornutum.tcases.openapi.testwriter.encoder.UriEncoder.Component.*;

import org.junit.Test;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Runs tests for {@link UriEncoder}
 */
public class UriEncoderTest
  {
  @Test
  public void whenComponentQuery()
    {
    assertEncodingFor( QUERY, "X/Y", "X/Y");
    assertEncodingFor( QUERY, "?X=Y&A=B", "%3FX%3DY%26A%3DB");
    assertEncodingFor( QUERY, "X[Y]", "X[Y]");
    assertEncodingFor( QUERY, "1,2,3", "1,2,3");
    assertEncodingFor( QUERY, "1|2|3", "1%7C2%7C3");
    assertEncodingFor( QUERY, "1 2 3", "1%202%203");
    }
  
  @Test
  public void whenComponentPath()
    {
    assertEncodingFor( PATH, "X/Y", "X/Y");
    assertEncodingFor( PATH, "?X=Y&A=B", "%3FX=Y&A=B");
    assertEncodingFor( PATH, "X[Y]", "X%5BY%5D");
    assertEncodingFor( PATH, "1,2,3", "1,2,3");
    assertEncodingFor( PATH, "1|2|3", "1%7C2%7C3");
    assertEncodingFor( PATH, "1 2 3", "1%202%203");
    }
  
  @Test
  public void whenComponentNone()
    {
    assertEncodingFor( NONE, "X/Y", "X/Y");
    assertEncodingFor( NONE, "?X=Y&A=B", "?X=Y&A=B");
    assertEncodingFor( NONE, "X[Y]", "X[Y]");
    assertEncodingFor( NONE, "1,2,3", "1,2,3");
    assertEncodingFor( NONE, "1|2|3", "1|2|3");
    assertEncodingFor( NONE, "1 2 3", "1 2 3");
    }

  private void assertEncodingFor( UriEncoder.Component component, String value, String encoded)
    {
    assertThat(
      String.format( "%s: '%s'", component, value),
      UriEncoder.uriEncoded( component, value),
      is( encoded));
    }
  }
