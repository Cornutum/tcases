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

import org.junit.Test;

/**
 * Runs tests for {@link RestServerTestWriter}.
 */
public class RestServerTestWriterTest extends TestWriterTest
  {
  @Test
  public void writeTest_0()
    {
    // Given...
    String testDefName = "testDef-0";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .inPackage( "org.examples")
      .extending( "org.examples.util.BaseClass")
      .inDir( getGeneratedTestDir())
      .build();

    MocoServerConfig config = new MocoServerConfigPojo();
    
    RestServerTestWriter testWriter = new RestServerTestWriter( config, new MockTestCaseWriter());
    
    // When...
    String results = toStdOut( () -> testWriter.writeTest( source, target));

    // Then
    verifyTest( testDefName, results);
    }
  }
