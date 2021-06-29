//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefReader;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils; 
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
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
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
   * Verifies that the "by path" test writer results for the given test match expectations.
   */
  protected void verifyTestsByPath( String testName, File actualResultsDir) throws Exception
    {
    File expectedResultsDir = getExpectedTestResultsDir( testName);
    if( acceptAsExpected())
      {
      File acceptedResultsDir = new File( saveExpectedDir_, expectedResultsDir.getName());
      if( acceptedResultsDir.exists())
        {
        FileUtils.cleanDirectory( acceptedResultsDir);
        }
      for( File actual : listJavaFiles( actualResultsDir))
        {
        FileUtils.copyFileToDirectory( actual, acceptedResultsDir);
        }
      }
    else
      {
      List<String> actuals = listJavaFiles( actualResultsDir).stream().map( File::getName).collect( toList());
      List<String> expecteds = listJavaFiles( expectedResultsDir).stream().map( File::getName).collect( toList());
      assertThat( testName + " results", actuals, containsMembers( expecteds));

      for( String results : actuals)
        {
        String actual;
        String expected;
        try
          {
          actual = FileUtils.readFileToString( new File( actualResultsDir, results), "UTF-8");
          expected = FileUtils.readFileToString( new File( expectedResultsDir, results), "UTF-8");
          }
        catch( Exception e)
          {
          throw new RuntimeException( String.format( "Can't read results for test=%s", testName), e);
          }

        assertTestResultsEqual( testName + ", " + results, actual, expected);
        }
      }
    }

  /**
   * Returns the *.java files in the given directory.
   */
  protected Collection<File> listJavaFiles( File dir)
    {
    return FileUtils.listFiles( dir, FileFilterUtils.suffixFileFilter( ".java"), null);
    }

  /**
   * Verifies that a TestWriterException occurs when the given Failable is executed.
   */
  protected void assertTestWriterException( Failable failable, String... expected)
    {
    assertFailure( TestWriterException.class, failable, expected);
    }

  /**
   * Verifies that a failure occurs when the given Failable is executed.
   */
  protected <F extends Throwable> void assertFailure( Class<F> failureType, Failable failable, String... expected)
    {
    expectFailure( failureType)
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
    return requestTestDefFor( getResourceClass(), testDefName);
    }

  /**
   * Returns the {@link RequestTestDef} object represented by the given document resource.
   */
  protected RequestTestDef requestTestDefFor( Class<?> resourceClass, String testDefName)
    {
    InputStream document = resourceClass.getResourceAsStream( String.format( "%s-Request-Cases.json", testDefName));
    assertThat( "Request cases for resource=" + testDefName, document, is( notNullValue()));
    
    try( RequestTestDefReader reader = new RequestTestDefReader( document))
      {
      return reader.getRequestTestDef();
      }
    }

  /**
   * Returns the specified standard {@link RequestTestDef} object.
   */
  protected RequestTestDef stdRequestTestDef( String testDefName)
    {
    return requestTestDefFor( TestWriterTest.class, testDefName);
    }

  /**
   * Returns the path to the OpenAPI spec represented by the given document resource.
   */
  protected File apiSpecFor( Class<?> resourceClass, String apiName)
    {
    String apiSpecFilename = String.format( "%s.json", apiName);
    InputStream document = resourceClass.getResourceAsStream( apiSpecFilename);
    assertThat( "OpenAPI spec for resource=" + apiName, document, is( notNullValue()));

    File apiSpecFile = new File( getResourceDir(), apiSpecFilename);
    if( !resourceClass.equals( getResourceClass()))
      {
      try
        {
        FileUtils.copyInputStreamToFile( document, apiSpecFile);
        }
      catch( Exception e)
        {
        throw new IllegalStateException( String.format( "Can't copy %s to %s", apiSpecFilename, getResourceDir()), e);
        }
      }
    
    return apiSpecFile;
    }

  /**
   * Returns the path to the OpenAPI spec represented by the given document resource.
   */
  protected File stdApiSpec( String apiName)
    {
    return apiSpecFor( TestWriterTest.class, apiName);
    }
  
  /**
   * Returns the expected test writer results for the specified {@link RequestTestDef}.
   */
  protected File getExpectedTestResults( String testDefName)
    {
    return new File( getResourceDir(), testDefName + "-Expected-Test.java");
    }
  
  /**
   * Returns the expected test writer results for the specified test.
   */
  protected File getExpectedTestResultsDir( String testName)
    {
    return new File( getResourceDir(), testName + "-Expected");
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
        IOUtils.closeQuietly( newOut, null);
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
   * Returns the location of a resource file subdirectory.
   */
  protected File getResourceDir( String subDir)
    {
    return new File( getResourceDir(), subDir);
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

  /**
   * A mock {@link TestCaseWriter}
   */
  public class MockTestCaseWriter implements TestCaseWriter
    {
    /**
     * Writes the dependencies for target test cases to the given stream.
     */
    public void writeDependencies( String testName, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Test case dependencies");
      }

    /**
     * Writes the declarations for target test cases to the given stream.
     */
    public void writeDeclarations( String testName, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Test case declarations");
      }
  
    /**
     * Writes a target test case to the given stream.
     */
    public void writeTestCase( String testName, URI testServer, RequestCase requestCase, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Given...");
      targetWriter.println( String.format( "// When testServer=%s...", testServer));
      targetWriter.println( "// Then...");
      }

    /**
     * Writes the closing for target test cases to the given stream.
     */
    public void writeClosing( String testName, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Test case closing");
      }
    }
  }
