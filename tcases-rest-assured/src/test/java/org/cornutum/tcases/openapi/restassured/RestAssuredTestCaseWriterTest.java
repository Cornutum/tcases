//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.apache.commons.io.FileUtils;
import org.cornutum.tcases.openapi.restassured.RestAssuredTestCaseWriter;
import org.cornutum.tcases.openapi.testwriter.JUnitTestWriter;
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
    String testDefName = "JSONPlaceholder-Api";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( testDefName)
      .inDir( getGeneratedTestDir())
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new RestAssuredTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    verifyTest( testDefName, FileUtils.readFileToString( testWriter.getTestFile( source, target), "UTF-8"));
    }
  }
