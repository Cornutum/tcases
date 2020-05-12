//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.junit.Test;

import java.io.File;

/**
 * Runs tests for {@link TestNgTestWriter}.
 */
public class TestNgTestWriterTest extends TestWriterTest
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
      .build();

    TestNgTestWriter testWriter = new TestNgTestWriter( new MockTestCaseWriter());
    
    // When...
    String results = toStdOut( () -> testWriter.writeTest( source, target));

    // Then
    verifyTest( testDefName, results);
    }
  
  /**
   * Returns the expected test writer results for the specified {@link RequestTestDef}.
   */
  protected File getExpectedTestResults( String testDefName)
    {
    return new File( getResourceDir(), testDefName + "-Expected-TestNG.java");
    }
  }
