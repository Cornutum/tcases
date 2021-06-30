//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.ApiTestCommand.Options;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.apache.commons.io.IOUtils;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;

/**
 * Runs tests for {@link ApiTestCommand}.
 */
public class ApiTestCommandByPathTest extends TestWriterTest
  {
  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths </TD> <TD> One </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void byPath_0() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outFile = new File( getResourceDir( "byPath_0"), "ActualTestClass");
    
    String[] args =
      {
        "-S",
        "-P", "/post",
        "-n", "MyTest",
        "-p", "org.cornutum.examples",
        "-f", outFile.getPath(),
        "-r", "20210517"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    verifyTestsByPath( "byPath_0", outFile.getParentFile());
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test 
  public void byPath_1() throws Exception
    {
    // Given...
    File apiFile = new File( getResourceDir( "byPath_1"), "OpenApiTest.json");
    
    String[] args =
      {
        "-n", "org.cornutum.examples.AllPaths",
        "-S",
        apiFile.getPath()
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    verifyTestsByPath( "byPath_1", apiFile.getParentFile());
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Many </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void byPath_2() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outDir = getResourceDir( "byPath_2");
    
    String[] args =
      {
        "-n", "org.cornutum.examples.SomePaths",
        "-f", "SomePathsTestCases.java",
        "-o", outDir.getPath(),
        "-P", "/posts/{userId}/{[attributes]},/users",
        "-r", "20210517",
        "-S"
      };

    // When...
    runWithStdIO( new Options( args), apiFile, null);
    
    // Then...
    verifyTestsByPath( "byPath_2", outDir);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths </TD> <TD> One </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void byPath_3() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outDir = getResourceDir( "byPath_3");

    String[] args =
      {
        "-p", "org.cornutum.examples",
        "-o", outDir.getPath(),
        "-P", "/posts/{userId}/{[attributes]}",
        "-S",
        apiFile.getPath()
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    verifyTestsByPath( "byPath_3", outDir);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void byPath_4() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outDir = getResourceDir( "byPath_4");
    
    String[] args =
      {
        "-S",
        "-n", "My Simple Test",
        "-o", outDir.getPath(),
        "-p", "org.cornutum.examples",
        "-r", "20210517"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    verifyTestsByPath( "byPath_4", outDir);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Many </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void byPath_5() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outFile = new File( getResourceDir( "byPath_5"), "Results");

    String[] args =
      {
        "-p", "org.cornutum.examples",
        "-f", outFile.getPath(),
        "-P", "/posts,/post,/users",
        "-S",
        apiFile.getPath()
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    verifyTestsByPath( "byPath_5", outFile.getParentFile());
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ByPath </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Many </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void byPath_6() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    
    String[] args =
      {
        "-S",
        "-p", "org.cornutum.examples",
        "-r", "20210517"
      };
    
    // When...
    StringBuffer stdOut = new StringBuffer();
    runWithStdIO( new Options( args), apiFile, stdOut);
        
    // Then...
    String testFileResults = stdOut.toString();
    verifyTest( "byPath_6", testFileResults);
    }

  /**
   * Run Tcases with the given options, using the given standard input/output.
   * If <CODE>stdIn</CODE> is non-null, redirect standard input to read from the given file.
   * If <CODE>stdOut</CODE> is non-null, redirect standard output to write to the given buffer.
   */
  private void runWithStdIO( Options options, File stdIn, StringBuffer stdOut) throws Exception
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

      ApiTestCommand.run( options);
      }
    finally
      {
      IOUtils.closeQuietly( newIn, null);
      IOUtils.closeQuietly( newOut, null);

      System.setIn( prevIn);
      System.setOut( prevOut);

      if( newOutBytes != null)
        {
        stdOut.append( new String( newOutBytes.toByteArray(), Charset.forName( "UTF-8")));
        }
      }
    }
  }
