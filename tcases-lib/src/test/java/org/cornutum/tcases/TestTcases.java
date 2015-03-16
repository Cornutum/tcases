//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.Tcases.Options;
import org.cornutum.tcases.io.SystemTestResources;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.io.FileUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.net.URL;

/**
 * Runs tests for {@link Tcases#main}.
 *
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
    Tcases.run( new Options( args));
        
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

    new Options( args);
    
    /* Must run interactively: uses standard input.
     
    // When...
    Tcases.run( new Options( args));
        
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

    new Options( args);

    /* Must run interactively: uses standard input
    // When...
    Tcases.run( new Options( args));
        
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
    Tcases.run( options);
        
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
    Tcases.run( new Options( args));
        
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

    new Options( args);
  
    /* Must run interactively: uses standard input
    // When...
    Tcases.run( new Options( args));
        
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
    try
      {
      Tcases.run( new Options( args));
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
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
    try
      {
      Tcases.run( new Options( args));
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
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
    try
      {
      Tcases.run( new Options( args));
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
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
    try
      {
      Options options = new Options( args);
      options.setWorkingDir( inFile.getParentFile());
      Tcases.run( options);
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
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
    try
      {
      Tcases.run( new Options( args));
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> No </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> None </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> NA </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_0() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-0");
    File outDir = getResourceFile( "run-transform-0-outDir");
    File outFile = new File( "run-transform-0.java");
    File outFilePath = new File( outDir, outFile.getName());
    File testDefFile = getResourceFile( "run-transform-0-test.xml");

    String[] args =
      {
        "-J",
        "-o", outDir.getPath(),
        "-f", outFile.getName(),
        "-t", testDefFile.getPath(),
        inFile.getPath()
      };
    
    // When...
    Tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFilePath.exists());
    }
  
  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> No </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> One </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_1() throws Exception
    {
    // Given...
    File transformFile = getResourceFile( "run-transform-1.xsl");

    String[] args =
      {
        "-x", transformFile.getPath(),
        "-p", "system=Run_Transform_1"
      };

    Options options = new Options( args);
    assertNotNull( options);
    
    /* Must run interactively: uses standard input.
    // When...
    Tcases.run( options);
        
    // Then...
    assertEquals( "Test def created", true, outFilePath.exists());
    */
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> Yes </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> Many </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_2() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-2");
    File outDir = getResourceFile( "run-transform-2-outDir");
    File outFile = getResourceFile( "run-transform-2.java");
    File outFilePath = new File( outDir, outFile.getName());
    File testDefFile = getResourceFile( "run-transform-2-test.xml");

    String[] args =
      {
        "-J",
        "-p", "throws=yes",
        "-o", outDir.getPath(),
        "-f", outFile.getPath(),
        "-t", testDefFile.getPath(),
        "-p", "class=Run_Transform_2",
        inFile.getPath()
      };
    
    // When...
    Tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFilePath.exists());
    }
  
  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> No </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> No </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> One </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_3() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-3.xml");
    File transformFile = getResourceFile( "run-transform-3.xsl");
    File outFilePath = getResourceFile( "run-transform-3.java");

    String[] args =
      {
        "-f", outFilePath.getName(),
        "-x", transformFile.getName(),
        "-p", "system=",
        inFile.getPath()
      };

    // When...
    Tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFilePath.exists());
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> No </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> One </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_4() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-4");
    File transformFile = getResourceFile( "run-transform-4.xsl");

    String[] args =
      {
        "-x", transformFile.getPath(),
        "-p", "=Run_Transform_1",
        inFile.getPath()
      };

    // When...
    Exception failure = null;
    try
      {
      Tcases.run( new Options( args));
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    assertEquals( "Usage exception", true, failure.getMessage().startsWith( "Usage: "));

    String cause = failure.getCause()==null? null : failure.getCause().getMessage();
    assertEquals( "Cause", "Invalid -p option: parameter name undefined", cause);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> No </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> No </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> None </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> NA </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_5() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-5");
    File transformFile = getResourceFile( "run-transform-5.xsl");

    String[] args =
      {
        "-x", transformFile.getPath(),
        inFile.getPath()
      };

    // When...
    try
      {
      Tcases.run( new Options( args));
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> No </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> One </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_6() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-6");
    File transformFile = getResourceFile( "run-transform-6.xsl");

    String[] args =
      {
        "-x", transformFile.getPath(),
        "-p", "Run_Transform_5",
        inFile.getPath()
      };

    // When...
    Exception failure = null;
    try
      {
      Tcases.run( new Options( args));
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    assertEquals( "Usage exception", true, failure.getMessage().startsWith( "Usage: "));

    String cause = failure.getCause()==null? null : failure.getCause().getMessage();
    assertEquals( "Cause", "Invalid -p option: must be name=value", cause);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> No </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> No </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> Many </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_7() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-7");
    File testDefFile = getResourceFile( "run-transform-7-Test.xml");
    File outFilePath = testDefFile;

    assertEquals( "Default output file exists", true, outFilePath.exists());
    
    String[] args =
      {
        "-x",
        "-p", "throws=yes",
        "-p", "class=Run_Transform_7",
        inFile.getPath()
      };

    // When...
    try
      {
      Tcases.run( new Options( args));
      fail( "Expected exception not thrown");
      }
    // Then...
    catch( Exception ignore)
      {
      }
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. run (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> <FONT color="red"> NotAllowed  </FONT> </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TD> </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> No </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> None </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> NA </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_8() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-8");
    File transformFile = getResourceFile( "run-transform-8.xsl");

    String[] args =
      {
        "-x", transformFile.getPath(),
        "-J",
        inFile.getPath()
      };

    // When...
    Exception failure = null;
    try
      {
      Tcases.run( new Options( args));
      }
    catch( Exception expected)
      {
      failure = expected;
      }

    // Then...
    assertEquals( "Exception thrown", true, failure != null);
    assertEquals( "Usage exception", true, failure.getMessage().startsWith( "Usage: "));

    String cause = failure.getCause()==null? null : failure.getCause().getMessage();
    assertEquals( "Cause", "Can't specify both -J and -x", cause);
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> inFile.defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> isJUnit </TD> <TD> Yes </TD> </TR>
   * <TR><TD> outDir.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> outFile.isTestFile </TD> <TD> NA </TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> Yes </TD> </TR>
   * <TR><TD> testFile.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.defined </TD> <TD> No </TD> </TR>
   * <TR><TD> transform.path.exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> transform.path.isAbsolute </TD> <TD> NA </TD> </TR>
   * <TR><TD> transformParams.assignsValue </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.count </TD> <TD> Many </TD> </TR>
   * <TR><TD> transformParams.nameDefined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> transformParams.valueDefined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_Transform_9() throws Exception
    {
    // Given...
    File inFile = getResourceFile( "run-transform-7");
    File outFilePath = getResourceFile( "runtransform7Test.java");

    String[] args =
      {
        "-J",
        "-p", "throws=yes",
        "-p", "class=Run_Transform_7",
        inFile.getPath()
      };
    
    // When...
    Tcases.run( new Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFilePath.exists());
    }

  @Test
  public void getTests_whenInputOnly() throws Exception
    {
    // Given...
    InputStream inputDef = getClass().getResourceAsStream( "tcases-Transform-Input.xml");
    
    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef);
    
    // Then...
    SystemTestDef expectedTestDef = testResources_.read( "tcases-Transform-Test.xml");
    assertEquals( "Test def generated", expectedTestDef, testDef);

    // When...
    ByteArrayOutputStream testDefOut = new ByteArrayOutputStream();
    Tcases.writeTests( testDef, testDefOut);

    // Then...
    ByteArrayInputStream testDefString = new ByteArrayInputStream( testDefOut.toByteArray());
    assertEquals( "Test def written", expectedTestDef, testResources_.read( testDefString));
    }

  @Test
  public void getTests_whenGenerator()
    {
    // Given...
    InputStream inputDef = getClass().getResourceAsStream( "tcases-Transform-Input.xml");
    InputStream genDef = getClass().getResourceAsStream( "tcases-Transform-Generators.xml");
    
    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, genDef, null);
    
    // Then...
    SystemTestDef expectedTestDef = testResources_.read( "tcases-Transform-Gen-Test.xml");
    assertEquals( "Test def generated", expectedTestDef, testDef);
    }

  @Test
  public void getTests_whenBaseTests()
    {
    // Given...
    InputStream inputDef = getClass().getResourceAsStream( "tcases-Transform-Input.xml");
    InputStream genDef = getClass().getResourceAsStream( "tcases-Transform-Generators.xml");
    InputStream baseDef = getClass().getResourceAsStream( "tcases-Transform-Test.xml");
    
    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, genDef, baseDef);
    
    // Then...
    SystemTestDef expectedTestDef = testResources_.read( "tcases-Transform-Regen-Test.xml");
    assertEquals( "Test def generated", expectedTestDef, testDef);
    }

  /**
   * Return the file for the given resource.
   */
  private File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  private SystemTestResources testResources_ = new SystemTestResources( getClass());
  }
