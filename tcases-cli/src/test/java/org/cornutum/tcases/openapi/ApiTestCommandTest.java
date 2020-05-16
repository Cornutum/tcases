//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.ApiTestCommand.Options;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;

/**
 * Runs tests for {@link ApiTestCommand}.
 */
public class ApiTestCommandTest extends TestWriterTest
  {
  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> restassured </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_0() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    File outFile = new File( getResourceDir(), "ActualTestClass");
    
    String[] args =
      {
        "-e", "restassured",
        "-n", "MyTest",
        "-p", "org.cornutum.examples",
        "-b", "MyBaseClass",
        "-f", outFile.getPath(),
        "-O", "get",
        "-c", ",ignore",
        "-r", "12345"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    File testFile = new File( outFile.getParent(), outFile.getName() + "Test.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-0", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> moco </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths </TD> <TD> One </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Log </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Log </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_1() throws Exception
    {
    // properties = apiSpec,moco,outputDir

    // Given...
    //
    //   Test-Type = moco
    //
    //   Exec-Type = Default
    //
    //   Test-Name = Default
    //
    //   Test-Package = Default
    //
    //   Base-Class = Simple
    //
    //   Output-File.Defined = No
    //
    //   Output-File.Path = (not applicable)
    //
    //   Output-Dir.Defined = Yes
    //
    //   Output-Dir.Exists = No
    //
    //   Moco-Test-Config = Defined
    //
    //   Paths = One
    //
    //   Operations = Default
    //
    //   Condition-Handler.Modelling = Log
    //
    //   Condition-Handler.Resolver = Log
    //
    //   Read-Only-Enforced = No
    //
    //   Random-Seed = Default
    //
    //   Max-Tries = Defined
    //
    //   Api-Spec.Defined = Yes
    //
    //   Api-Spec.Path = Absolute
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> junit </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> restassured </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Many </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Many </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Default </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_2() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    File outFile = new File( "examples/myTestCase");
    File outDir = new File( getResourceDir(), "java/org/cornutum");
    
    String[] args =
      {
        "-t", "junit",
        "-e", "restassured",
        "-n", "MyTest",
        "-b", "org.cornutum.examples.MyBaseClass",
        "-f", outFile.getPath(),
        "-o", outDir.getPath(),
        "-P", "/posts/{attributes}, /post",
        "-O", "TRACE, PATCH",
        "-c", "ignore,",
        "-r", "13214566"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    File testFile = new File( outDir, outFile.getPath() + ".java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-2", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> testng </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Relative </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_3() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    
    String[] args =
      {
        "-t", "testng",
        "-p", "org.cornutum.examples",
        "-b", "org.cornutum.testwriter.BaseTest",
        "-O", "TRACE",
        "-c", ",ignore",
        "-m", "10",
        apiFile.getPath()
      };
    
    // When...
    StringBuffer stdOut = new StringBuffer();
    runWithStdIO( new Options( args), null, stdOut);
        
    // Then...
    String testFileResults = stdOut.toString();
    verifyTest( "api-test-3", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> restassured </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> One </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Log </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Log </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_4() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    File outFile = new File( getResourceDir(), "ActualName");
    
    String[] args =
      {
        "-e", "restassured",
        "-n", "org.cornutum.examples.MyTest",
        "-p", "org.cornutum",
        "-b", "MyBaseClass",
        "-f", outFile.getPath(),
        "-P", "/posts",
        "-c", "log,log",
        "-r", "20200515"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    File testFile = new File( outFile.getParent(), outFile.getName() + "Test.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-4", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> moco </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Many </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Many </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Default </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_5() throws Exception
    {
    // properties = apiSpec,moco,outputDir,testNameFqn

    // Given...
    //
    //   Test-Type = moco
    //
    //   Exec-Type = Default
    //
    //   Test-Name = Fqn
    //
    //   Test-Package = Default
    //
    //   Base-Class = Default
    //
    //   Output-File.Defined = No
    //
    //   Output-File.Path = (not applicable)
    //
    //   Output-Dir.Defined = Yes
    //
    //   Output-Dir.Exists = No
    //
    //   Moco-Test-Config = Defined
    //
    //   Paths = Many
    //
    //   Operations = Many
    //
    //   Condition-Handler.Modelling = Ignore
    //
    //   Condition-Handler.Resolver = Default
    //
    //   Read-Only-Enforced = No
    //
    //   Random-Seed = Default
    //
    //   Max-Tries = Defined
    //
    //   Api-Spec.Defined = Yes
    //
    //   Api-Spec.Path = Absolute
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> junit </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> restassured </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_6() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    File outFile = new File( "MyTests");
    File outDir = new File( getResourceDir(), "java/org/cornutum/examples/tests");
    
    String[] args =
      {
        "-t", "junit",
        "-e", "restassured",
        "-f", outFile.getPath(),
        "-o", outDir.getPath(),
        "-O", "options",
        "-c", ",ignore",
        "-r", "20200515"
      };

    try
      {
      // When...
      runWithStdIO( new Options( args), apiFile, null);
        
      // Then...
      File testFile = new File( outDir, outFile.getPath() + ".java");
      String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
      verifyTest( "api-test-6", testFileResults);
      }
    finally
      {
      outFile.delete();
      }
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> testng </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Default </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> One </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Log </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Log </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Relative </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_7() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    
    String[] args =
      {
        "-t", "testng",
        "-n", "Why Not?",
        "-p", "org.cornutum.examples",
        "-P", "/post",
        "-m", "1234",
        apiFile.getPath()
      };
    
    // When...
    StringBuffer stdOut = new StringBuffer();
    runWithStdIO( new Options( args), null, stdOut);
        
    // Then...
    String testFileResults = stdOut.toString();
    verifyTest( "api-test-7", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> restassured </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Fqn </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Many </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Many </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Default </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Default </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_8() throws Exception
    {
    // Given...
    File apiFile = stdApiSpec( "OpenApiTest");
    File outFile = new File( getResourceDir(), "ActualTest");
    
    String[] args =
      {
        "-e", "restassured",
        "-n", "org.cornutum.examples.MyTest",
        "-p", "org.cornutum",
        "-b", "org.cornutum.utils.MyBaseClass",
        "-f", outFile.getPath(),
        "-P", "/post,/post/{userId}/{approved} ,/posts",
        "-O", "put ,PATCH,delete",
        "-c", "ignore",
        "-r", "20200515"
      };
    
    // When...
    runWithStdIO( new Options( args), apiFile, null);
        
    // Then...
    File testFile = new File( outFile.getParent(), outFile.getName() + ".java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-8", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Fail </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> <FONT color="red"> Yes  </FONT> </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_9() throws Exception
    {
    // Given...
    File apiFile = apiSpecFor( getResourceClass(), "read-only-enforced");
    File outDir = new File( getResourceDir(), "failures");
    
    String[] args =
      {
        "-n", "ReadOnlyTest",
        "-b", "MyBaseClass",
        "-o", outDir.getPath(),
        "-c", "fail,ignore",
        "-R",
        "-m", "1",
        apiFile.getPath()
      };

    ApiTestCommand.run( new Options( args));
    
    // When...
    //    expectFailure( RuntimeException.class)
    //      .when( () -> ApiTestCommand.run( new Options( args)));
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> moco </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> <FONT color="red"> Fail  </FONT> </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_10() throws Exception
    {
    // properties = apiSpec,moco,outputDir

    // Given...
    //
    //   Test-Type = moco
    //
    //   Exec-Type = Default
    //
    //   Test-Name = Simple
    //
    //   Test-Package = Default
    //
    //   Base-Class = Simple
    //
    //   Output-File.Defined = No
    //
    //   Output-File.Path = (not applicable)
    //
    //   Output-Dir.Defined = Yes
    //
    //   Output-Dir.Exists = No
    //
    //   Moco-Test-Config = Defined
    //
    //   Paths = Default
    //
    //   Operations = One
    //
    //   Condition-Handler.Modelling = Fail
    //
    //   Condition-Handler.Resolver = Ignore
    //
    //   Read-Only-Enforced = No
    //
    //   Random-Seed = Default
    //
    //   Max-Tries = Defined
    //
    //   Api-Spec.Defined = Yes
    //
    //   Api-Spec.Path = Absolute
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> moco </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> <FONT color="red"> Undefined  </FONT> </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_11() throws Exception
    {
    // properties = apiSpec,moco

    // Given...
    //
    //   Test-Type = moco
    //
    //   Exec-Type = Default
    //
    //   Test-Name = Simple
    //
    //   Test-Package = Undefined
    //
    //   Base-Class = Simple
    //
    //   Output-File.Defined = No
    //
    //   Output-File.Path = (not applicable)
    //
    //   Output-Dir.Defined = No
    //
    //   Output-Dir.Exists = (not applicable)
    //
    //   Moco-Test-Config = Defined
    //
    //   Paths = Default
    //
    //   Operations = One
    //
    //   Condition-Handler.Modelling = Default
    //
    //   Condition-Handler.Resolver = Ignore
    //
    //   Read-Only-Enforced = No
    //
    //   Random-Seed = Default
    //
    //   Max-Tries = Defined
    //
    //   Api-Spec.Defined = Yes
    //
    //   Api-Spec.Path = Absolute
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> moco </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> <FONT color="red"> Fail  </FONT> </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_12() throws Exception
    {
    // properties = apiSpec,moco,outputDir

    // Given...
    //
    //   Test-Type = moco
    //
    //   Exec-Type = Default
    //
    //   Test-Name = Simple
    //
    //   Test-Package = Default
    //
    //   Base-Class = Simple
    //
    //   Output-File.Defined = No
    //
    //   Output-File.Path = (not applicable)
    //
    //   Output-Dir.Defined = Yes
    //
    //   Output-Dir.Exists = No
    //
    //   Moco-Test-Config = Defined
    //
    //   Paths = Default
    //
    //   Operations = One
    //
    //   Condition-Handler.Modelling = Default
    //
    //   Condition-Handler.Resolver = Fail
    //
    //   Read-Only-Enforced = No
    //
    //   Random-Seed = Default
    //
    //   Max-Tries = Defined
    //
    //   Api-Spec.Defined = Yes
    //
    //   Api-Spec.Path = Absolute
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Test-Type </TD> <TD> moco </TD> </TR>
   * <TR><TD> Exec-Type </TD> <TD> Default </TD> </TR>
   * <TR><TD> Test-Name </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Test-Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Base-Class </TD> <TD> Simple </TD> </TR>
   * <TR><TD> Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Output-Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> <FONT color="red"> Undefined  </FONT> </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> One </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Ignore </TD> </TR>
   * <TR><TD> Read-Only-Enforced </TD> <TD> No </TD> </TR>
   * <TR><TD> Random-Seed </TD> <TD> Default </TD> </TR>
   * <TR><TD> Max-Tries </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Api-Spec.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Api-Spec.Path </TD> <TD> Absolute </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_13() throws Exception
    {
    // properties = apiSpec,moco,outputDir

    // Given...
    //
    //   Test-Type = moco
    //
    //   Exec-Type = Default
    //
    //   Test-Name = Simple
    //
    //   Test-Package = Default
    //
    //   Base-Class = Simple
    //
    //   Output-File.Defined = No
    //
    //   Output-File.Path = (not applicable)
    //
    //   Output-Dir.Defined = Yes
    //
    //   Output-Dir.Exists = No
    //
    //   Moco-Test-Config = Undefined
    //
    //   Paths = Default
    //
    //   Operations = One
    //
    //   Condition-Handler.Modelling = Default
    //
    //   Condition-Handler.Resolver = Ignore
    //
    //   Read-Only-Enforced = No
    //
    //   Random-Seed = Default
    //
    //   Max-Tries = Defined
    //
    //   Api-Spec.Defined = Yes
    //
    //   Api-Spec.Path = Absolute
    
    // When...

    // Then...
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
  }
