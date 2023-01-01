//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.ApiTestCommand.Options;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;
import static org.cornutum.tcases.CommandTest.runWithStdIO;

import org.junit.Test;

import java.io.File;

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
    runWithStdIO( () -> ApiTestCommand.run( new Options( args)), apiFile, null);
        
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
    runWithStdIO( () -> ApiTestCommand.run( new Options( args)), apiFile, null);
    
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
    runWithStdIO( () -> ApiTestCommand.run( new Options( args)), apiFile, null);
        
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
    runWithStdIO( () -> ApiTestCommand.run( new Options( args)), apiFile, stdOut);
        
    // Then...
    String testFileResults = stdOut.toString();
    verifyTest( "byPath_6", testFileResults);
    }
  }
