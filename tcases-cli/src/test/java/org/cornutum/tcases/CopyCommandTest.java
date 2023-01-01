//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.CopyCommand.Options;

import org.apache.commons.io.FileUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;

/**
 * Runs tests for {@link CopyCommand}.
 */
public class CopyCommandTest extends CommandTest
  {
  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Copy (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> No </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenDestDirDefined() throws Exception
    {
    // Given...
    File inputDef = getResourceFile( "copy-0");
    File genDef = new File( "copy-0-GenDef");
    File testDef = new File( "copy-0-Test");
    String destName = "MyCopy";
    File destDir = getResourceFile( "myCopyDir");

    FileUtils.deleteDirectory( destDir);
    
    String[] args =
      {
        "-T", "xml",
        "-g", genDef.getPath(),
        "-t", testDef.getName(),
        "--toName", destName,
        "--toDir", destDir.getName(),
        inputDef.getName() 
      };
    Options options = new Options( args);
    options.setWorkingDir( inputDef.getParentFile());
    
    // When...
    CopyCommand.run( options);
        
    // Then...
    File copyInputDef = new File( destDir, String.format( "%s-Input.xml", destName));
    File copyGenDef = new File( destDir, String.format( "%s-Generators.xml", destName));
    File copyTestDef = new File( destDir, String.format( "%s-Test.xml", destName));

    assertThat( "Input def copied", copyInputDef.exists(), is( true));
    assertThat( "Gen def copied", copyGenDef.exists(), is( false));
    assertThat( "Test def copied", copyTestDef.exists(), is( true));
    }

  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Copy (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> xml </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> No </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputTypeXml() throws Exception
    {
    // Given...
    File inputDef = getResourceFile( "copy-1-Input.xml");

    String[] args =
      {
        "--toType", "json",
        inputDef.getPath()
      };
    Options options = new Options( args);
    
    // When...
    CopyCommand.run( options);
        
    // Then...
    String destName = "copy-1";
    File inputDir = inputDef.getParentFile();
    File copyInputDef = new File( inputDir, String.format( "%s-Input.json", destName));
    File copyGenDef = new File( inputDir, String.format( "%s-Generators.json", destName));
    File copyTestDef = new File( inputDir, String.format( "%s-Test.json", destName));

    assertThat( "Input def copied", copyInputDef.exists(), is( true));
    assertThat( "Gen def copied", copyGenDef.exists(), is( true));
    assertThat( "Test def copied", copyTestDef.exists(), is( false));
    }

  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Copy (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> json </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> No </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputTypeJson() throws Exception
    {
    // Given...
    File inputDef = getResourceFile( "copy-2-Input.json");
    File genDef = new File( "copy-2-GenDef");
    File testDef = getResourceFile( "copy-2-Tests");
    String destName = "other";
    File destDir = getResourceFile( "otherDir");

    FileUtils.deleteDirectory( destDir);
    
    String[] args =
      {
        "-T", "json",
        "-g", genDef.getName(),
        "-t", testDef.getPath(),
        "--toName", destName,
        "--toDir", destDir.getPath(),
        inputDef.getPath()
      };
    Options options = new Options( args);
    
    // When...
    CopyCommand.run( options);
        
    // Then...
    File copyInputDef = new File( destDir, String.format( "%s-Input.json", destName));
    File copyGenDef = new File( destDir, String.format( "%s-Generators.json", destName));
    File copyTestDef = new File( destDir, String.format( "%s-Test.json", destName));

    assertThat( "Input def copied", copyInputDef.exists(), is( true));
    assertThat( "Gen def copied", copyGenDef.exists(), is( false));
    assertThat( "Test def copied", copyTestDef.exists(), is( true));
    }

  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Copy (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Defined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenOptionsHelp() throws Exception
    {
    // Given...
    File inputDef = getResourceFile( "copy-0");
    File genDef = new File( "copy-0-GenDef");
    File testDef = new File( "copy-0-Test");
    String destName = "MyCopy";
    File destDir = getResourceFile( "myCopyDir");

    FileUtils.deleteDirectory( destDir);
    
    String[] args =
      {
        "-help",
        "-T", "xml",
        "-g", genDef.getPath(),
        "-t", testDef.getName(),
        "--toName", destName,
        "--toDir", destDir.getName(),
        inputDef.getName()
      };
    
    // When...
    StringBuffer errFile = new StringBuffer();
    expectFailure( HelpException.class)
      .when( () -> runWithStdErr( () -> new Options( args), errFile));
        
    // Then...
    assertThat( "Standard error", errFile.toString(), is( not( nullValue())));

    File copyInputDef = new File( destDir, String.format( "%s-Input.xml", destName));
    File copyGenDef = new File( destDir, String.format( "%s-Generators.xml", destName));
    File copyTestDef = new File( destDir, String.format( "%s-Test.xml", destName));

    assertThat( "Input def copied", copyInputDef.exists(), is( false));
    assertThat( "Gen def copied", copyGenDef.exists(), is( false));
    assertThat( "Test def copied", copyTestDef.exists(), is( false));
    }

  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Copy (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenOptionsVersion() throws Exception
    {
    // Given...
    File inputDef = getResourceFile( "copy-0");
    File genDef = new File( "copy-0-GenDef");
    File testDef = new File( "copy-0-Test");
    String destName = "MyCopy";
    File destDir = getResourceFile( "myCopyDir");

    FileUtils.deleteDirectory( destDir);
    
    String[] args =
      {
        "-v",
        "-T", "xml",
        "-g", genDef.getPath(),
        "-t", testDef.getName(),
        "--toName", destName,
        "--toDir", destDir.getName(),
        inputDef.getName()        
      };
    Options options = new Options( args);

        // When...
    StringBuffer outFile = new StringBuffer();
    runWithStdIO( () -> CopyCommand.run( options), null, outFile);
        
    // Then...
    assertThat( "Standard output", outFile.toString(), is( not( nullValue())));

    File copyInputDef = new File( destDir, String.format( "%s-Input.xml", destName));
    File copyGenDef = new File( destDir, String.format( "%s-Generators.xml", destName));
    File copyTestDef = new File( destDir, String.format( "%s-Test.xml", destName));

    assertThat( "Input def copied", copyInputDef.exists(), is( false));
    assertThat( "Gen def copied", copyGenDef.exists(), is( false));
    assertThat( "Test def copied", copyTestDef.exists(), is( false));
    }

  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Copy (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> No </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> Default </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenDestinationSame() throws Exception
    {
    // Given...
    File inputDef = getResourceFile( "copy-0");
    File genDef = getResourceFile( "copy-0-GenDef");
    File testDef = getResourceFile( "copy-0-Test");

    String[] args =
      {
        "-T", "xml",
        "-g", genDef.getPath(),
        "-t", testDef.getName(),
        inputDef.getName()
      };
    Options options = new Options( args);
    options.setWorkingDir( inputDef.getParentFile());
    
    // When...
    CopyCommand.run( options);
        
    // Then...
    assertThat( "Input def copied", inputDef.exists(), is( true));
    assertThat( "Gen def copied", genDef.exists(), is( false));
    assertThat( "Test def copied", testDef.exists(), is( true));
    }

  /**
   * Tests {@link CopyCommand#run} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Copy (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Options.showVersion </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Options.help </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> InputDef.Defined </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> InputDef.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Generators </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDef.Base-Tests </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.contentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.genDef </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.testDef </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destName </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destDir.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.destDir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Args.Destinaton </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputDefMissing()
    {
    // Given...
    
    String[] args = {};
    
    // When...
    expectFailure( IllegalArgumentException.class)
      .when( () ->  new Options( args))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Invalid command line argument. For all command line details, use the -help option."));
        assertThat( "Failure", failure.getCause().getMessage(), is( "No project inputDef specified"));
        });
    }
  }
