//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.apache.commons.io.FileUtils;
import org.cornutum.tcases.openapi.moco.MocoServerConfig;
import org.cornutum.tcases.openapi.moco.MocoServerTestWriter;
import org.cornutum.tcases.openapi.moco.RestServerTestWriter;
import org.cornutum.tcases.openapi.restassured.RestAssuredTestCaseWriter;
import org.cornutum.tcases.openapi.testwriter.JavaTestTarget;
import org.cornutum.tcases.openapi.testwriter.TestSource;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.junit.Test;

/**
 * Runs tests for {@link RestAssuredTestCaseWriter}
 */
public class RestAssuredTestCaseWriterTest extends TestWriterTest
  {
  @Test
  public void writeTest_0() throws Exception
    {
    // Given...
    String testDefName = "OpenApiTest";
    
    TestSource source =
      TestSource.from( stdRequestTestDef( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( testDefName)
      .inDir( getGeneratedTestDir())
      .build();

    MocoServerConfig serverConfig = MocoServerConfig.resource( stdMocoServerConfig( testDefName)).build();

    MocoServerTestWriter testWriter = new RestServerTestWriter( serverConfig, new RestAssuredTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    verifyTest( testDefName, FileUtils.readFileToString( testWriter.getTestFile( source, target), "UTF-8"));
    }
  }
