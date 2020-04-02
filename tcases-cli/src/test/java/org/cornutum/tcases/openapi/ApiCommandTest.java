//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.io.SystemTestResources;
import org.cornutum.tcases.openapi.ApiCommand.Options;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import java.nio.charset.Charset;

/**
 * Runs tests for {@link ApiCommand}.
 */
public class ApiCommandTest
  {
  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Client </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Test </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Fail </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> Html </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_0() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-0.json");
    File outFile = new File( apiFile.getParentFile(), "api-run-0-Responses-Test.htm");

    outFile.delete();
    
    String[] args =
      {
        "-C",
        "-c", "fail",
        "-W",
        "-H",
        apiFile.getPath()
      };
    
    // When...
    ApiCommand.run( new Options( args));
        
    // Then...
    assertThat( "Output model created", outFile.exists(), is( true));

    // Given...
    apiFile = getResourceFile( "api-run-0-fail.json");
    outFile = new File( apiFile.getParentFile(), "api-run-0-fail-Requests-Test.htm");

    outFile.delete();
    
    String[] failArgs =
      {
        "-S",
        "-c", "fail",
        "-W",
        "-H",
        apiFile.getPath()
      };
    
    expectFailure( OpenApiException.class)
      .when( () -> ApiCommand.run( new Options( failArgs)))
      .then( failure -> assertThat( "Failure", failure.getMessage(), is( "Error processing AllOf, /allOf, POST, param0, allOf, oneOf, oneOf[0]")));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Server </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> JUnit </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_1() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-1.json");
    StringBuffer outFile = new StringBuffer();
    
    String[] args =
      {
        "-S",
        "-c", "ignore",
        "-o", apiFile.getParent(),
        "-R",
        "-J"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, outFile);
        
    // Then...
    assertThat( "Output model created", outFile.length() > 0, is( true));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Default </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Input </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Log </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_2() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-2.yaml");
    File outDir = new File( apiFile.getParentFile(), "newDir");
    File outFile = new File( outDir, "api-run-2-Input.json");

    FileUtils.deleteDirectory( outDir);
    
    String[] args =
      {
        "-I",
        "-c", "log",
        "-f", new File( apiFile.getParentFile(), outFile.getName()).getPath(),
        "-o", outDir.getPath(),
        "-W",
        "-T", "yaml"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    assertThat( "Output model created", inputResources_.readJson( outFile), is( notNullValue()));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Client </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Test </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> JUnit </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Relative </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_3() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-3.yml");
    String outFileName = "api-run-3-Test.json";
    File outFile = new File( apiFile.getParentFile(), "apirun3Test.java");

    outFile.delete();
    
    String[] args =
      {
        "-C",
        "-f", outFileName, 
        "-R",
        "-J",
        apiFile.getPath()
      };
    
    // When...
    ApiCommand.run( new Options( args));
        
    // Then...
    assertThat( "Output model created", outFile.exists(), is( true));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Server </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Fail </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> None </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_4() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-4.json");
    File outFile = new File( apiFile.getParentFile(), "api-run-4-Requests-Test.json");

    outFile.delete();
    
    String[] args =
      {
        "-S",
        "-c", "fail",
        apiFile.getPath()
      };
    
    // When...
    ApiCommand.run( new Options( args));
        
    // Then...
    assertThat( "Output model created", testResources_.readJson( outFile), is( notNullValue()));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Default </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Test </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> Html </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_5() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-5.json");
    File outFileDir = new File( apiFile.getParentFile(), "someDir");
    File outFilePath = new File( outFileDir, "api-run-5-Test.json");
    File outFile = getResourceFile( "api-run-5-Test.htm");

    outFile.delete();
    
    String[] args =
      {
        "-c", "ignore",
        "-f", outFilePath.getPath(),
        "-o", outFile.getParent(),
        "-R",
        "-H"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    assertThat( "Output model created", outFile.exists(), is( true));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Client </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Log </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> None </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Relative </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_6() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-6.yaml");
    File outFile = new File( apiFile.getParentFile(), "api-run-6-Test.json");

    outFile.delete();
    
    String[] args =
      {
        "-C",
        "-c", "log",
        "-f", outFile.getName(),
        "-W",
        apiFile.getPath()
      };
    
    // When...
    ApiCommand.run( new Options( args));
        
    // Then...
    assertThat( "Output model created", testResources_.readJson( outFile), is( notNullValue()));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Server </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Test </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> Custom </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_7() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-7.json");
    File outFile = new File( String.format( "%s/%s", apiFile.getParent(), "transformed"), "api-run-7.java");
    File transformFile = getResourceFile( "api-run-7.xsl");
    
    String[] args =
      {
        "-S",
        "-f", outFile.getPath(),
        "-x", transformFile.getPath()
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    assertThat( "Output model created", outFile.exists(), is( true));
    }

  /**
   * Tests {@link ApiCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Perspective </TD> <TD> Default </TD> </TR>
   * <TR><TD> Model-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler </TD> <TD> Fail </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Write-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Transformer </TD> <TD> Custom </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_8() throws Exception
    {
    // Given...
    File apiFile = getResourceFile( "api-run-8.json");
    File outDir = new File( apiFile.getParent(), "transformed");
    File outFile = new File( outDir, "api-run-8-Requests-Test.json");
    File transformFile = getResourceFile( "api-run-8.xsl");
    
    String[] args =
      {
        "-o", outDir.getPath(),
        "-x", transformFile.getName(),
        "-p", "system=Foo",
        apiFile.getPath()
      };
    
    // When...
    ApiCommand.run( new Options( args));
        
    // Then...
    assertThat( "Output model created", outFile.exists(), is( true));
    }

  /**
   * Return the file for the given resource.
   */
  private File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  /**
   * Run Tcases with the given options, using the given standard input/output.
   * If <CODE>stdIn</CODE> is non-null, redirect standard input to read from the given file.
   * If <CODE>stdOut</CODE> is non-null, redirect standard output to write to the given buffer.
   */
  private void runWithStdIO( Options options, File stdIn, StringBuffer stdOut)
    {
    InputStream prevIn = System.in;
    PrintStream prevOut = System.out;

    InputStream newIn = null;
    PrintStream newOut = null;
    ByteArrayOutputStream newOutBytes = null;
    
    try
      {
      if( stdIn != null)
        {
        System.setIn( (newIn = new FileInputStream( stdIn)));
        }

      if( stdOut != null)
        {
        stdOut.delete( 0, stdOut.length());
        System.setOut( (newOut = new PrintStream( (newOutBytes = new ByteArrayOutputStream()))));
        }

      ApiCommand.run( options);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't run with options=" + options, e);
      }
    finally
      {
      IOUtils.closeQuietly( newIn);
      IOUtils.closeQuietly( newOut);

      System.setIn( prevIn);
      System.setOut( prevOut);

      if( newOutBytes != null)
        {
        stdOut.append( new String( newOutBytes.toByteArray(), Charset.forName( "UTF-8")));
        }
      }
    }

  private SystemInputResources inputResources_ = new SystemInputResources( getClass());
  private SystemTestResources testResources_ = new SystemTestResources( getClass());
  }
