//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefReader;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.cornutum.hamcrest.ExpectedFailure.Failable;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Base class for {@link TestWriter} tests.
 */
public abstract class TestWriterTest
  {
  /**
   * Verifies that the test writer results for the given request test definition match expectations.
   */
  protected void verifyTest( String testDefName, String testWriterResults)
    {
    File expectedResults = getExpectedTestResults( testDefName);
    if( acceptAsExpected())
      {
      updateTestResults( expectedResults.getName(), testWriterResults);
      }
    else
      {
      String expected;
      try
        {
        expected = FileUtils.readFileToString( expectedResults, "UTF-8");
        }
      catch( Exception e)
        {
        throw new RuntimeException( String.format( "Can't read expected results for testDef=%s", testDefName), e);
        }

      assertTestResultsEqual( testDefName, testWriterResults, expected);
      }
    }

  /**
   * Verifies that a TestWriterException occurs when the given Failable is executed.
   */
  protected void assertTestWriterException( Failable failable, String... expected)
    {
    expectFailure( TestWriterException.class)
      .when( failable)
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat( "Causes", causes.build().collect( toList()), listsMembers( expected));
        });
    }

  /**
   * Reports a failure if the actual test results are not equal to the expected results.
   */
  protected void assertTestResultsEqual( String testDefName, String actual, String expected)
    {
    int diffStart = StringUtils.indexOfDifference( actual, expected);
    if( diffStart >= 0)
      {
      int sampleSize = 16;
      int sampleStart = Math.max( 0, diffStart - sampleSize/2);
      int actualSampleEnd = Math.min( diffStart + sampleSize/2, actual.length());
      int expectedSampleEnd = Math.min( diffStart + sampleSize/2, expected.length());

      fail(
        String.format(
          "Unexpected results for %s starting at index=%s -- expected '%s%s%s' but was '%s%s%s'",
          testDefName,
          diffStart,
          sampleStart == 0? "" : "...",
          expected.substring( sampleStart, expectedSampleEnd).replaceAll( "\\n", "\\\\n"),
          expectedSampleEnd == expected.length()? "" : "...",
          sampleStart == 0? "" : "...",
          actual.substring( sampleStart, actualSampleEnd).replaceAll( "\\n", "\\\\n"),
          actualSampleEnd == actual.length()? "" : "..."));
      }
    }

  /**
   * Updates expected test writer results.
   */
  private void updateTestResults( String expectedResultsName, String actualResults)
    {
    try
      {
      File expectedResults = new File( saveExpectedDir_, expectedResultsName);
      FileUtils.write( expectedResults, actualResults, "UTF-8");
      }
    catch( Exception e)
      {
      throw new RuntimeException( String.format( "Can't update expectedResults=%s", expectedResultsName), e);
      }
    }

  /**
   * Returns the {@link RequestTestDef} object represented by the given document resource.
   */
  protected RequestTestDef requestTestDefFor( String testDefName)
    {
    InputStream document = getResourceClass().getResourceAsStream( String.format( "%s-Request-Cases.json", testDefName));
    assertThat( "Request cases for resource=" + testDefName, document, is( notNullValue()));
    
    try( RequestTestDefReader reader = new RequestTestDefReader( document))
      {
      return reader.getRequestTestDef();
      }
    }

  /**
   * Verifies that the test writer results for the given request test definition match expectations.
   */
  protected File getExpectedTestResults( String testDefName)
    {
    return new File( getResourceDir(), testDefName + "-Expected-Test.java");
    }

  /**
   * Runs the given Runnable and returns the results printed to standard output
   */
  protected String toStdOut( Runnable runnable)
    {
    try
      {
      PrintStream prevOut = System.out;
      PrintStream newOut = null;
      ByteArrayOutputStream newOutBytes = null;

      try
        {
        newOutBytes = new ByteArrayOutputStream();
        newOut = new PrintStream( newOutBytes, true, "UTF-8");
        System.setOut( newOut);

        runnable.run();
        }
      finally
        {
        IOUtils.closeQuietly( newOut);
        System.setOut( prevOut);
        }

      return new String( newOutBytes.toByteArray(), "UTF-8");
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get results from standard output", e);
      }
    }

  /**
   * Returns the location of resource files.
   */
  protected File getResourceDir()
    {
    try
      {
      return new File( getResourceClass().getResource( ".").toURI().getPath());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get resource directory path", e);
      }
    }

  /**
   * Returns the class used to locate test resources.
   */
  protected Class<?> getResourceClass()
    {
    return getClass();
    }

  /**
   * Returns true if all generated results are automatically accepted.
   */
  private boolean acceptAsExpected()
    {
    return saveExpectedDir_ != null;
    }

  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);
  }
