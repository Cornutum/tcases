//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

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
    tcases.run( new Tcases.Options( args));
        
    // Then...
    assertEquals( "Test def created", true, outFile.exists());
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
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
   * </TABLE>
   * </P>
   */
  @Test
  public void run_1() throws Exception
    {
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
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
   * </TABLE>
   * </P>
   */
  @Test
  public void run_2() throws Exception
    {
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
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
   * </TABLE>
   * </P>
   */
  @Test
  public void run_3() throws Exception
    {
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
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
   * </TABLE>
   * </P>
   */
  @Test
  public void run_4() throws Exception
    {
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. run (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
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
   * </TABLE>
   * </P>
   */
  @Test
  public void run_5() throws Exception
    {
    }

  /**
   * Tests {@link Tcases#run run()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. run (Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
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
   * <TR><TD> outDir.path.isDirectory </TD> <TD><FONT color=red> No </FONT></TD></TR>
   * <TR><TD> outFile.defined </TD> <TD> Yes </TD></TR>
   * <TR><TD> outFile.path.exists </TD> <TD> No </TD></TR>
   * <TR><TD> outFile.path.isAbsolute </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void run_6() throws Exception
    {
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
    }

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
    tcases.run( new Tcases.Options( args));
        
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
