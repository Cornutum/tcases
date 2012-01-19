//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.Tcases.Options;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.net.URL;

/**
 * Runs tests for {@link Tcases#main}.
 *
 * @version $Revision$, $Date$
 */
public class TestTcases
  {
  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> No </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> NA </TD></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> ForInputNone </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> withXml </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> No </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_0() throws Exception
    {
    // Given...
    File outDir = getResourceFile( "run-0-outDir");
    File testDefFile = getResourceFile( "run-0-test-other.xml");
    File inFile = getResourceFile( "run-0");
    File genFile = new File( inFile.getParent(), inFile.getName() + "-Generators.xml");
    File outFile = new File( outDir, testDefFile.getName());

    FileUtils.deleteDirectory( outDir);
    testDefFile.delete();
    genFile.delete();
    
    String[] args =
      {
        "-n",
        "-o", outDir.getPath(),
        "-t", testDefFile.getPath(),
        inFile.getPath()
      };
    
    // When...
    Tcases tcases = new Tcases();
    tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFile.exists());
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> Yes </TD></TR>
   * <TR><TD> extendTests </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_1() throws Exception
    {
    // Given...
    File testDefFile = getResourceFile( "run-1-test-other.xml");
    File genFile = getResourceFile( "run-1-gen-other.xml");
    testDefFile.delete();
    
    String[] args =
      {
        "-n",
        "-c", "12345",
        "-r", "45678",
        "-g", genFile.getPath(),
        "-t", testDefFile.getName()
      };

    /* Must run interactively: uses standard input.
     
    // When...
    Tcases tcases = new Tcases();
    tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFile.exists());
    */
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> No </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> NA </TD></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> Standard </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> No </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_2() throws Exception
    {
    // Given...
    File outDir = getResourceFile( "run-2-outDir");
    File testDefFile = getResourceFile( "run-2-test-other.xml");

    FileUtils.deleteDirectory( outDir);
    outDir.mkdirs();
    
    String[] args =
      {
        "-o", outDir.getPath(),
        "-t", testDefFile.getName()
      };

    /* Must run interactively: uses standard input
    // When...
    Tcases tcases = new Tcases();
    tcases.run( new Options( args));
        
    // Then...
    File outFile = new File( outDir, testDefFile.getName());
    assertEquals( "Test def created", true, outFile.exists());
    */
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> Yes </TD></TR>
   * <TR><TD> extendTests </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> withInputXml </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> No </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_3() throws Exception
    {
    // Given...
    File testDefFile = getResourceFile( "run-3-test-other.xml");
    File genFile = getResourceFile( "run-3-gen-other.xml");
    File inFile = getResourceFile( "run-3-Input.xml");
    File outFile = testDefFile;

    String[] args =
      {
        "-c", "12345",
        "-g", genFile.getName(),
        "-t", testDefFile.getPath(),
        "run-3"
      };

    Options options = new Options( args);
    options.setWorkingDir( inFile.getParentFile());
    
    // When...
    Tcases tcases = new Tcases();
    tcases.run( options);
        
    // Then...
    assertEquals( "Test def created", true, outFile.exists());
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> No </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> NA </TD></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> ForInputExists </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> asDefined </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_4() throws Exception
    {
    // Given...
    File outDir = getResourceFile( "run-4-outDir");
    File inFile = getResourceFile( "run-4.xml");
    File outFile = new File( outDir, "run-4-Test.xml");

    FileUtils.deleteDirectory( outDir);

    String[] args =
      {
        "-n",
        "-o", outDir.getPath(),
        "-r", "12345",
        inFile.getPath()
      };

    // When...
    Tcases tcases = new Tcases();
    tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFile.exists());
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> Yes </TD></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_5() throws Exception
    {
    // Given...
    File genFile = getResourceFile( "run-5-gen-other.xml");
    
    String[] args =
      {
        "-g", genFile.getPath(),
        "-r", "12345",
        "-c", "45678"
      };

    /* Must run interactively: uses standard input
    // When...
    Tcases tcases = new Tcases();
    tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFile.exists());
    */
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> No </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> NA </TD></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> Standard </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD><FONT color=red> No </FONT></TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> No </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_6() throws Exception
    {
    // Given...
    File outDir = getResourceFile( "run-6-outDir");
    File testDefFile = getResourceFile( "run-6-test-other.xml");
    File inFile = getResourceFile( "run-6-Input.xml");

    outDir.delete();
    outDir.createNewFile();
    
    String[] args =
      {
        "-o", outDir.getPath(),
        "-n",
        "-t", testDefFile.getName(),
        inFile.getPath()
      };

    // When...
    Tcases tcases = new Tcases();
    Exception failure = null;
    try
      {
      tcases.run( new Options( args));
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. run (Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> extendTests </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD><FONT color=red> No </FONT></TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> withInputXml </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_7() throws Exception
    {
    // Given...
    File testDefFile = getResourceFile( "run-7-test-other.xml");
    File genFile = getResourceFile( "run-7-gen-other.xml");
    File inFile = getResourceFile( "run-7-Input.xml");

    String[] args =
      {
        "-g", genFile.getName(),
        "-t", testDefFile.getPath(),
        "run-7"
      };

    Options options = new Options( args);
    options.setWorkingDir( inFile.getParentFile());

    // When...
    Tcases tcases = new Tcases();
    Exception failure = null;
    try
      {
      tcases.run( options);
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. run (Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> ForInputExists </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD><FONT color=red> No </FONT></TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_8() throws Exception
    {
    // Given...
    File outDir = getResourceFile( "run-8-outDir");
    File inFile = getResourceFile( "run-8.xml");

    FileUtils.deleteDirectory( outDir);

    String[] args =
      {
        "-n",
        "-o", outDir.getPath(),
        inFile.getPath()
      };

    // When...
    Tcases tcases = new Tcases();
    Exception failure = null;
    try
      {
      tcases.run( new Options( args));
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. run (Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD><FONT color=red> No </FONT></TD></TR>
   * <TR><TD> extendTests </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> withInputXml </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> No </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_9() throws Exception
    {
    // Given...
    File testDefFile = getResourceFile( "run-9-test-other.xml");
    File genFile = getResourceFile( "run-9-gen-other.xml");
    File inFile = getResourceFile( "run-9-Input.xml");

    String[] args =
      {
        "-c",
        "-g", genFile.getName(),
        "-t", testDefFile.getPath(),
        "run-3"
      };

    // When...
    Tcases tcases = new Tcases();
    Exception failure = null;
    try
      {
      Options options = new Options( args);
      options.setWorkingDir( inFile.getParentFile());
    
      tcases.run( options);
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> defaultTupleSize.defined </TD> <TD> No </TD></TR>
   * <TR><TD> defaultTupleSize.isNumber </TD> <TD> NA </TD></TR>
   * <TR><TD> extendTests </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.default </TD> <TD> ForInputExists </TD></TR>
   * <TR><TD> genFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> genFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> genFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> inFile.path.exists </TD> <TD> asDefined </TD></TR>
   * <TR><TD> inFile.path.isAbsolute </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outDir.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outDir.path.isDirectory </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> NA </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD></TR>
   * <TR><TD> seed.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> seed.isNumber </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_10() throws Exception
    {
    // Given...
    File outDir = getResourceFile( "run-10-outDir");
    File inFile = getResourceFile( "run-10.xml");

    FileUtils.deleteDirectory( outDir);

    String[] args =
      {
        "-n",
        "-o", outDir.getPath(),
        "-r", "NaN",
        inFile.getPath()
      };

    // When...
    Tcases tcases = new Tcases();
    Exception failure = null;
    try
      {
      tcases.run( new Options( args));
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    }

  /**
   * Tests {@link Tcases#run run()} using the input definition for the Tcases command line.
   */
  //  @Test
  public void run_ForTcases() throws Exception
    {
    // Given...
    String[] args =
      {
        "-n",
        getResourceFile( "tcases-Input.xml").getPath()
      };
    
    // When...
    Tcases tcases = new Tcases();
    tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, getResourceFile( "tcases-Test.xml").exists());
    }

  /**
   * Return the file for the given resource.
   */
  private File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  }
