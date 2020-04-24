//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.openapi.testwriter.JavaTestTarget;
import org.cornutum.tcases.openapi.testwriter.TestSource;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;

/**
 * Runs tests for {@link RestServerTestWriter}.
 */
public class RestServerTestWriterTest extends TestWriterTest
  {
  @Test
  public void writeTest_0() throws Exception
    {
    // Given...
    String testDefName = "RequestTestDef0";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( testDefName)
      .inDir( getGeneratedTestDir())
      .build();

    MocoServerConfig config = new MocoServerConfigPojo();
    
    RestServerTestWriter testWriter = new RestServerTestWriter( config, new MockTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    File outFile = new File( getGeneratedTestDir(), testDefName + "Test.java");
    String outFileResults = FileUtils.readFileToString( outFile, "UTF-8");
    verifyTest( testDefName, outFileResults);
    }
  }
