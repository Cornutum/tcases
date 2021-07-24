//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.ApiTestCommand.Options;
import org.cornutum.tcases.openapi.moco.CertConfigResource;
import org.cornutum.tcases.openapi.moco.MocoServerConfig;
import org.cornutum.tcases.openapi.moco.MocoTestConfig;
import org.cornutum.tcases.openapi.moco.MocoTestConfigWriter;
import org.cornutum.tcases.openapi.resolver.ResolverException;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
    File outFile = new File( getResourceDir(), "ActualTestClass");
    
    String[] args =
      {
        "-e", "restassured",
        "-n", "MyTest",
        "-p", "org.cornutum.examples",
        "-b", "MyBaseClass",
        "-f", outFile.getPath(),
        "-u", "0",
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outDir = new File( getResourceDir(), "java/org/cornutum/moco");

    File testConfigFile = new File( getResourceDir(), "mocoRestServer.json");
    MocoTestConfig testConfig =
      MocoTestConfig.builder( "RestServer")
      .serverConfig( MocoServerConfig.file( new File( getResourceDir(), "myRestServerConfig.json")).build())
      .build();
    writeMocoTestConfig( testConfig, testConfigFile);
    
    String[] args =
      {
        "-t", "moco",
        "-b", "MyBaseClass",
        "-o", outDir.getPath(),
        "-M", testConfigFile.getPath(),
        "-P", "/post",
        "-c", "log,log",
        "-m", "123",
        apiFile.getPath()
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    File testFile = new File( outDir, "OpenAPIRequestTestCasesTest.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-1", testFileResults);
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
    
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
    ApiTestCommand.run( new Options( args));
        
    // Then...
    File testFile = new File( apiFile.getParentFile(), "OpenAPIRequestTestCasesTest.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outDir = new File( getResourceDir(), "java/org/cornutum/moco");

    File testConfigFile = new File( getResourceDir(), "mocoHttpsServer.json");
    MocoTestConfig testConfig =
      MocoTestConfig.builder( "HttpsServer")
      .serverConfig( MocoServerConfig.resource( "myMocoServerConfig").port( 9999).forEachTest().build())
      .certConfig( new CertConfigResource( "myCertificate", "myCert.cks", "kss!", "css!"))
      .build();
    writeMocoTestConfig( testConfig, testConfigFile);
    
    String[] args =
      {
        "-t", "moco",
        "-n", "org.cornutum.moco.MyMocoTest",
        "-o", outDir.getPath(),
        "-M", testConfigFile.getName(),
        "-P", "/post, /posts",
        "-O", "get, delete, put",
        "-c", "ignore,",
        "-m", "123",
        apiFile.getPath()
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    File testFile = new File( outDir, "MyMocoTest.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-5", testFileResults);
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
   * <TR><TD> Timeout </TD> <TD> Valid </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
    File outFile = new File( "MyTests");
    File outDir = new File( getResourceDir(), "java/org/cornutum/examples/tests");
    
    String[] args =
      {
        "-t", "junit",
        "-e", "restassured",
        "-f", outFile.getPath(),
        "-o", outDir.getPath(),
        "-u", "12345",
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
   * <TR><TD> Timeout </TD> <TD> Valid </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
    
    String[] args =
      {
        "-t", "testng",
        "-n", "Why Not?",
        "-p", "org.cornutum.examples",
        "-P", "/post",
        "-m", "1234",
        "-u", "12345",
        apiFile.getPath()
      };
    
    // When...
    ApiTestCommand.run( new Options( args));
        
    // Then...
    File testFile = new File( apiFile.getParentFile(), "WhyNotTest.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    File apiFile = stdApiDef( "OpenApiTest");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths </TD> <TD> Default </TD> </TR>
   * <TR><TD> Operations </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Modelling </TD> <TD> Default </TD> </TR>
   * <TR><TD> Condition-Handler.Resolver </TD> <TD> Default </TD> </TR>
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
    File apiFile = apiDefFor( getResourceClass(), "read-only-enforced");
    File outDir = new File( getResourceDir(), "java/org/cornutum/readonly");
    
    String[] args =
      {
        "-n", "ReadOnlyTest",
        "-b", "MyBaseClass",
        "-o", outDir.getPath(),
        "-R",
        "-m", "1",
        apiFile.getPath()
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    File testFile = new File( outDir, "ReadOnlyTest.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-9", testFileResults);
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. run (<FONT color="red">Failure</FONT>) </TH></TR>
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
   * <TR><TD> Moco-Test-Config </TD> <TD> (not applicable) </TD> </TR>
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
    // Given...
    File apiFile = apiDefFor( getResourceClass(), "modelling-condition");
    File outDir = new File( getResourceDir(), "conditions");
    
    String[] args =
      {
        "-p", "org.cornutum",
        "-o", outDir.getPath(),
        "-c", "fail,ignore",
        apiFile.getPath()
      };

    // When...
    assertFailure(
      OpenApiException.class,
      () -> ApiTestCommand.run( new Options( args)),
      "Error processing Numbers, /numbers, POST, param0",
      "minimum=1000 is greater than maximum=99");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    
    String[] args = new String[0];

    // When...
    assertTestWriterException(
      () -> runWithStdIO( new Options( args), apiFile, null),
      "JUnitTestWriter[]: Can't write test for TestSource[RequestTestDef[OpenAPI Request Test Cases]]",
      "Can't write test=OpenAPIRequestTestCases",
      "No package defined for target=JavaTestTarget[package=<null>,STDOUT]");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    // Given...
    File apiFile = apiDefFor( getResourceClass(), "resolver-condition");
    File outFile = new File( getResourceDir(), "java/org/cornutum/MyTest.java");
    
    String[] args =
      {
        "-n", "Condition",
        "-f", outFile.getPath(),
        "-c", ",fail",
        apiFile.getPath()
      };

    // When...
    assertFailure(
      ResolverException.class,
      () -> ApiTestCommand.run( new Options( args)),
      "Error processing RequestCaseDef[2,param0.Items.Size='> 1',POST,/array,SUCCESS], param0, value, unique item[1] of 6",
      "Unable to resolve a value after 10000 tries");
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
   * <TR><TD> Timeout </TD> <TD> Default </TD> </TR>
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
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    File outDir = new File( getResourceDir(), "java/org/cornutum/moco");

    File testConfigFile = new File( getResourceDir(), "mocoHttpServer.json");
    MocoTestConfig testConfig =
      MocoTestConfig.builder( "HttpServer")
      .serverConfig( MocoServerConfig.resource( "myMocoServerConfig").build())
      .build();
    writeMocoTestConfig( testConfig, testConfigFile);
    
    String[] args =
      {
        "-t", "moco",
        "-n", "MyMocoTest",
        "-b", "MyBaseClass",
        "-o", outDir.getPath(),
        "-O", "get",
        "-c", ",ignore",
        apiFile.getPath()
      };

    assertTestWriterException(
      () -> ApiTestCommand.run( new Options( args)),
      "Can't create Moco test writer",
      "No Moco server test configuration defined");
    }

  /**
   * Tests {@link ApiTestCommand#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. run (<FONT color="red">Failure</FONT>) </TH></TR>
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
   * <TR><TD> Timeout </TD> <TD> <FONT color="red">Invalid</FONT> </TD> </TR>
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
  public void run_14() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    
    String[] args =
      {
        "-t", "testng",
        "-n", "Why Not?",
        "-p", "org.cornutum.examples",
        "-P", "/post",
        "-m", "1234",
        "-u", "wtf?",
        apiFile.getPath()
      };

    assertFailure(
      IllegalArgumentException.class,
      () -> ApiTestCommand.run( new Options( args)),
      "Invalid command line argument. For all command line details, use the -help option.",
      "Invalid timeout",
      "For input string: \"wtf?\"");
    }

  @Test
  public void whenPrintToStdout() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    
    String[] args =
      {
        "-p", "org.cornutum.examples",
        "-r", "99999"
      };
    
    // When...
    StringBuffer stdOut = new StringBuffer();
    runWithStdIO( new Options( args), apiFile, stdOut);
        
    // Then...
    String testFileResults = stdOut.toString();
    verifyTest( "api-test-stdout", testFileResults);
    }

  @Test
  public void whenSourceExamples() throws Exception
    {
    // Given...
    File apiFile = stdApiDef( "OpenApiTest");
    
    String[] args =
      {
        "-X",
        "-n", "org.cornutum.examples.OpenApiExamplesTest",
        apiFile.getPath()
      };
    
    // When...
    ApiTestCommand.run( new Options( args));
        
    // Then...
    File testFile = new File( apiFile.getParentFile(), "OpenApiExamplesTest.java");
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    verifyTest( "api-test-examples", testFileResults);
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

  /**
   * Writes the given {@link MocoTestConfig} to a file.
   */
  private void writeMocoTestConfig( MocoTestConfig testConfig, File destFile)
    {
    destFile.getParentFile().mkdirs();
    try( MocoTestConfigWriter writer = new MocoTestConfigWriter( new FileOutputStream( destFile)))
      {
      writer.write( testConfig);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't write Moco test configuration to %s", destFile), e);
      }
    }
  }
