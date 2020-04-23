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

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

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

  /**
   * Returns the <CODE>generated-test-sources</CODE> directory for this test.
   */
  protected File getGeneratedTestDir()
    {
    File resourceDir = getResourceDir();
    List<String> resourcePath = Arrays.stream( resourceDir.getPath().split( "/")).collect( toList());

    int targetEnd = resourcePath.indexOf( "target") + 1;
    int packageStart = targetEnd + 1;

    List<String> generatedTestPath =
      Stream.concat(
        Stream.concat(
          resourcePath.subList( 0, targetEnd).stream(),
          Arrays.asList( "generated-test-sources", "java").stream()),
        resourcePath.subList( packageStart, resourcePath.size()).stream())
      .collect( toList());

    File generatedTestDir = new File( generatedTestPath.stream().collect( joining( "/")));
    generatedTestDir.mkdirs();

    return generatedTestDir;
    }

  }
