//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.ApiTestCommand.Options;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;

/**
 * Runs tests for {@link ApiTestCommand}.
 */
public class ApiTestCommandServersTest extends TestWriterTest
  {
  @Test
  public void servers_0() throws Exception
    {
    verifyRootDefinedServer( 0, "contains=My");
    }
     
  @Test
  public void servers_1() throws Exception
    {
    verifyRootUndefinedServer( 1, "index=3");
    }
  
  @Test
  public void servers_2() throws Exception
    {
    verifyRootDefinedServer( 2, "contains=   ");
    }
  
  @Test
  public void servers_3() throws Exception
    {
    verifyRootDefinedServer( 3, "index=1");
    }
     
  @Test
  public void servers_4() throws Exception
    {
    verifyRootUndefinedServer( 4, "uri=http://myhost.com/");
    }
  
  @Test
  public void servers_5() throws Exception
    {
    verifyRootDefinedServer( 5, "contains=local");
    }
     
  @Test
  public void servers_6() throws Exception
    {
    verifyRootUndefinedServer( 6, "index=1");
    }
  
  @Test
  public void servers_7() throws Exception
    {
    verifyRootDefinedServer( 7, "contains=production root");
    }
  
  @Test
  public void servers_8() throws Exception
    {
    verifyRootDefinedServer( 8, "index=2");
    }
  
  @Test
  public void servers_9() throws Exception
    {
    assertFailure(
      IllegalArgumentException.class,
      () -> verifyRootDefinedServer( 9, "index=two"),
      "Invalid command line argument. For all command line details, use the -help option.",
      "Invalid base URI",
      "For input string: \"two\""); 
    }
  
  @Test
  public void servers_10() throws Exception
    {
    assertFailure(
      IllegalArgumentException.class,
      () -> verifyRootUndefinedServer( 10, "uri=http://"),
      "Invalid command line argument. For all command line details, use the -help option.",
      "Invalid base URI",
      "Expected authority at index 7: http://"); 
    }
  
  @Test
  public void servers_11() throws Exception
    {
    assertFailure(
      IllegalArgumentException.class,
      () -> verifyRootUndefinedServer( 11, "contains="),
      "Invalid command line argument. For all command line details, use the -help option.",
      "Invalid base URI",
      "'contains=' is not a valid server expression"); 
    }
  
  @Test
  public void servers_12() throws Exception
    {
    assertFailure(
      IllegalArgumentException.class,
      () -> verifyRootUndefinedServer( 12, "index = 0"),
      "Invalid command line argument. For all command line details, use the -help option.",
      "Invalid base URI",
      "'index = 0' is not a valid server expression"); 
    }

  /**
   * Verify expected results using the given server expression when root servers are defined.
   */
  private void verifyRootDefinedServer( int testId, String serverExpr) throws Exception
    {
    verifyServer( "api-test-servers-defined", testId, serverExpr);
    }

  /**
   * Verify expected results using the given server expression when root servers are undefined.
   */
  private void verifyRootUndefinedServer( int testId, String serverExpr) throws Exception
    {
    verifyServer( "api-test-servers-undefined", testId, serverExpr);
    }

  /**
   * Verify expected results using the given server expression.
   */
  private void verifyServer( String apiName, int testId, String serverExpr) throws Exception
    {
    // Given...
    File apiFile = apiSpecFor( getResourceClass(), apiName);
    String testName = String.format( "ApiTestServers_%s_Test", testId);
    
    String[] args =
      {
        "-p", "org.cornutum.examples",
        "-n", testName,
        "-B", serverExpr,
        apiFile.getPath()        
      };

    // When...
    ApiTestCommand.run( new Options( args));
    
    // Then...
    String testFileName = String.format( "%s.java", testName);
    File testFile = new File( apiFile.getParentFile(), testFileName);
    String testFileResults = FileUtils.readFileToString( testFile, "UTF-8");
    
    String testFileExpected = String.format( "api-test-servers-%s", testId);
    verifyTest( testFileExpected, testFileResults);
    }
  }
