//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.io;

import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.SystemTestDefMatcher;
import org.cornutum.tcases.io.SystemTestDocReader;
import org.cornutum.tcases.io.SystemTestJsonReader;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

/**
 * Runs tests for {@link TcasesOpenApiIO}
 */
public class TcasesOpenApiIOTest
  {
  @Test
  public void writeOperationRequestTests()
    {
    verifyRequestTests( "../operations-1");
    }
  
  @Test
  public void writeObjectRequestTests()
    {
    verifyRequestTests( "../object-6");
    }
  
  private void verifyRequestTests( String apiResource)
    {
    // Given...
    InputStream api = getClass().getResourceAsStream( apiResource + ".json");
    
    // When...
    ByteArrayOutputStream systemTestOut = new ByteArrayOutputStream();
    TcasesOpenApiIO.writeTests( TcasesOpenApiIO.getRequestTests( api), systemTestOut);
    
    // Then...
    SystemTestDef testDef;
    ByteArrayInputStream systemTestIn = new ByteArrayInputStream( systemTestOut.toByteArray());
    try( SystemTestJsonReader reader = new SystemTestJsonReader( systemTestIn))
      {
      testDef = reader.getSystemTestDef();
      }

    SystemTestDef expectedTestDef;
    try( SystemTestDocReader reader = new SystemTestDocReader( getClass().getResourceAsStream( apiResource + "-Expected-Test.xml")))
      {
      expectedTestDef = reader.getSystemTestDef();
      }

    assertThat( apiResource + ", test cases", testDef, matches( new SystemTestDefMatcher( expectedTestDef)));
    }

  }
