//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.test.ResponsesDef;
import org.cornutum.tcases.util.ToString;

import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.IOUtils.closeQuietly;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.util.List;
import java.util.Optional;

/**
 * Writes the source code for a test that executes API requests.
 */
public abstract class TestWriter<S extends TestSource, T extends TestTarget>
  {
  /**
   * Creates a new TestWriter instance.
   */
  protected TestWriter( TestCaseWriter testCaseWriter)
    {
    testCaseWriter_ = testCaseWriter;
    }

  /**
   * Creates a test that executes the request test cases defined by the given {@link TestSource source}
   * and writes the result to the given {@link TestTarget target}.
   */
  public void writeTest( S source, T target)
    {
    String testName = getTestName( source, target);
    File targetFile = getTargetFile( target, testName);

    File targetDir =
      Optional.ofNullable( targetFile)
      .flatMap( file -> Optional.ofNullable( file.getParentFile()))
      .map( File::getAbsoluteFile)
      .orElse( null);
    if( targetDir != null && !(targetDir.exists() || targetDir.mkdirs()))
      {
      throw new TestWriterException( String.format( "Can't create targetDir=%s", targetDir));
      }

    OutputStream fileStream = null;
    IndentedWriter targetWriter = null;
    try
      {
      fileStream =
        targetFile == null
        ? null
        : new FileOutputStream( targetFile);

      OutputStream targetStream =
        Optional.ofNullable( fileStream)
        .orElse(
          Optional.ofNullable( target.getOutput())
          .orElse( System.out));

      targetWriter = new IndentedWriter( targetStream);
      targetWriter.setIndent( 4);

      prepareTestCases( source.getRequestCases());
      writeProlog( target, testName, targetWriter);
      writeTestCases( target, testName, source.getRequestCases(), targetWriter);
      writeEpilog( target, testName, targetWriter);
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "Can't write test=%s", testName), e);
      }
    finally
      {
      if( targetWriter != null)
        {
        targetWriter.flush();
        }
      
      closeQuietly( fileStream, null);
      }

    Optional.ofNullable( source.getResponses())
      .ifPresent( responses -> writeResponsesDef( responses, targetFile, getTestResourceDir( targetFile, target.getResourceDir())));
    }

  /**
   * Returns the test file written for the given source and target. Returns null if the test is written to a stream.
   */
  public File getTestFile( S source, T target)
    {
    return getTargetFile( target, getTestName( source, target));
    }

  /**
   * Returns the test name derived from the given source and target.
   */
  public String getTestName( S source, T target)
    {
    return
      getTestName(
        Optional.ofNullable( getTestBaseName( source, target))
        .orElseThrow( () -> new TestWriterException( String.format( "No test name defined by source=%s, target=%s", source, target))));
    }

  /**
   * Returns the test name derived from the given base name.
   */
  protected abstract String getTestName( String baseName);

  /**
   * Returns the base test name defined by the given source and target.
   */
  protected String getTestBaseName( S source, T target)
    {
    return
      Optional.ofNullable( target.getName())
      .orElse(
        Optional.ofNullable( target.getFile())
        .map( file -> getBaseName( file.getName()))
        .orElse( source.getApi()));
    }

  /**
   * Returns the resource directory derived from the given source and target
   */
  public File getTestResourceDir( S source, T target)
    {
    String testName = getTestName( source, target);
    File targetFile = getTargetFile( target, testName);
    return getTestResourceDir( targetFile, target.getResourceDir());
    }

  /**
   * Returns the resource directory derived from the given target file and resource directory options.
   */
  protected File getTestResourceDir( File targetFile, File resourceDir)
    {
    return
      targetFile == null?
      // Test cases go to standard output when the OpenAPI definition comes from standard input.
      // In this case, the OpenAPI definition can't be reprocessed to generate resources.
      null :

      Optional.ofNullable( resourceDir)
      .orElse(
        Optional.ofNullable( targetFile.getParentFile())
        .orElse( new File( ".")));
    }

  /**
   * Returns the target file defined by the given target.
   */
  protected File getTargetFile( T target, String testName)
    {
    File targetFile = target.getFile();
    File targetDir = target.getDir();

    return
      targetFile == null && targetDir == null?
      null :

      targetDir == null?
      targetFile :

      targetFile == null?
      new File( targetDir, testName.replaceAll( "[/<>|:& \\\\]+", "-")) :

      targetFile.isAbsolute()?
      targetFile :
      
      new File( targetDir, targetFile.getPath());
    }
  
  /**
   * Prepare this writer to handle the given request cases.
   */
  protected void prepareTestCases( List<RequestCase> requestCases)
    {
    getTestCaseWriter().prepareTestCases( requestCases);
    }

  /**
   * Writes the target test prolog to the given stream.
   */
  protected void writeProlog( T target, String testName, IndentedWriter targetWriter)
    {
    writeOpening( target, testName, targetWriter);

    writeDependencies( target, testName, targetWriter);
    getTestCaseWriter().writeDependencies( testName, targetWriter);

    writeDeclarations( target, testName, targetWriter);
    getTestCaseWriter().writeDeclarations( testName, targetWriter);
    }

  /**
   * Writes the target test opening to the given stream.
   */
  protected abstract void writeOpening( T target, String testName, IndentedWriter targetWriter);

  /**
   * Writes the target test dependencies to the given stream.
   */
  protected abstract void writeDependencies( T target, String testName, IndentedWriter targetWriter);

  /**
   * Writes the target test declarations to the given stream.
   */
  protected abstract void writeDeclarations( T target, String testName, IndentedWriter targetWriter);

  /**
   * Writes the target test cases to the given stream.
   */
  protected void writeTestCases( T target, String testName, List<RequestCase> requestCases, IndentedWriter targetWriter)
    {
    requestCases.stream().forEach( requestCase -> writeTestCase( target, testName, requestCase, targetWriter));
    }

  /**
   * Writes a target test case to the given stream.
   */
  protected void writeTestCase( T target, String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    getTestCaseWriter().writeTestCase( testName, getTestServer( requestCase), requestCase, targetWriter);
    }

  /**
   * Writes the target test epilog to the given stream.
   */
  protected void writeEpilog( T target, String testName, IndentedWriter targetWriter)
    {
    getTestCaseWriter().writeClosing( testName, targetWriter);
    writeClosing( target, testName, targetWriter);
    }

  /**
   * Writes the target test closing to the given stream.
   */
  protected abstract void writeClosing( T target, String testName, IndentedWriter targetWriter);

  /**
   * Writes the given responses definitions to a resource file associated with the test target file.
   */
  protected void writeResponsesDef( ResponsesDef responses, File targetFile, File resourceDir)
    {
    if( resourceDir != null)
      {
      if( !(resourceDir.exists() || resourceDir.mkdirs()))
        {
        throw new TestWriterException( String.format( "Can't create resourceDir=%s", resourceDir));
        }

      File resourceFile = new File( resourceDir, String.format( "%s-Responses.json", getBaseName( targetFile.getName())));

      try( OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( resourceFile), "UTF-8"))
        {
        ResponsesDef.write( responses, writer);
        }
      catch( Exception e)
        {
        throw new TestWriterException( String.format( "Can't write responses definition to resourceFile=%s", resourceFile), e);
        }
      }
    }

  /**
   * Returns the {@link TestCaseWriter} for this test.
   */
  protected TestCaseWriter getTestCaseWriter()
    {
    return testCaseWriter_;
    }

  /**
   * Returns a URI for the API server used by the given test case. If non-null, this supersedes
   * the server URI defined by this {@link RequestCase request case}.
   */
  protected URI getTestServer( RequestCase requestCase)
    {
    // By default, none defined.
    return null;
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }

  private final TestCaseWriter testCaseWriter_;
  }
