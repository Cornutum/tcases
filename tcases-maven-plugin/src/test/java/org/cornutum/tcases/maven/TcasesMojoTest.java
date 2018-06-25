package org.cornutum.tcases.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;

import org.cornutum.tcases.TcasesCommand;
import org.junit.Rule;
import org.junit.Test;
import static org.junit.Assert.*;

import java.io.File;
import java.util.Arrays;

/**
 * Runs tests for the {@link TcasesMojo} plugin.
 */
public class TcasesMojoTest
  {
  @Test
  public void withConfigDefault() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertEquals( "Input defs", 1, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 1, expectedTestDefs.length);

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedTestDef = new File( expectedInputDef.getParent(), TcasesCommand.getProjectName( expectedInputDef) + "-Test.xml");
    assertEquals( "Test def", expectedTestDef.getPath(), expectedTestDefs[0]);
    }
  
  @Test
  public void withConfigCustom() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-custom");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "custom");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/custom");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/Input.xml", "**/InputModel");
    assertEquals( "Input defs", 2, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 2, expectedTestDefs.length);
    }
  
  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Other </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Other </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Wildcard </TD> <TD> One </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Relative </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> TransformDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformParams.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Junit </TD> <TD> No </TD> </TR>
   * <TR><TD> NewTests </TD> <TD> No </TD> </TR>
   * <TR><TD> Seed.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> DefaultTupleSize.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputPatternsMany() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-many");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "tcases/input");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases/output");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "Input.xml", "*/The*Input.xml", "**/Other.xml");
    assertEquals( "Input defs", 3, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 3, expectedTestDefs.length);
    }

  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TestDef.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> GenDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TransformDef.Relative </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformParams.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Junit </TD> <TD> Yes </TD> </TR>
   * <TR><TD> NewTests </TD> <TD> Default </TD> </TR>
   * <TR><TD> Seed.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> DefaultTupleSize.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenOutputJUnit() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-junit");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertEquals( "Input defs", 1, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 1, expectedTestDefs.length);

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedTestDef = new File( expectedInputDef.getParent(), TcasesCommand.getProjectName( expectedInputDef) + "Test.java");
    assertEquals( "Test def", expectedTestDef.getPath(), expectedTestDefs[0]);
    }

  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> One </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TestDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Wildcard </TD> <TD> One </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Relative </TD> <TD> No </TD> </TR>
   * <TR><TD> TransformDef.Wildcard </TD> <TD> One </TD> </TR>
   * <TR><TD> TransformDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformParams.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Junit </TD> <TD> Default </TD> </TR>
   * <TR><TD> NewTests </TD> <TD> No </TD> </TR>
   * <TR><TD> Seed.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> DefaultTupleSize.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputPatternsOne() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-one");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/Project-*-Inputs.xml");
    assertEquals( "Input defs", 3, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 3, expectedTestDefs.length);

    Arrays.sort( expectedInputDefs);
    Arrays.sort( expectedTestDefs);
    for( int i = 0; i < expectedInputDefs.length; i++)
      {
      File expectedInputDef = new File( expectedInputDefs[i]);
      File expectedTestDef = new File( expectedInputDef.getParent(), "Tests-For-" + TcasesCommand.getProjectName( expectedInputDef) + ".xml");
      assertEquals( "Test def", expectedTestDef.getPath(), expectedTestDefs[i]);
      }
    }

  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> None </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> NA </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Relative </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> TransformParams.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> Junit </TD> <TD> NA </TD> </TR>
   * <TR><TD> NewTests </TD> <TD> NA </TD> </TR>
   * <TR><TD> Seed.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> DefaultTupleSize.Defined </TD> <TD> NA </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenInputDefsNone() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-none");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    assertEquals( "Out dir created", false, expectedOutDir.exists());
    }

  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> TestDef.Defined </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Wildcard </TD> <TD> NA </TD> </TR>
   * <TR><TD> TestDef.Exists </TD> <TD> NA </TD> </TR>
   * <TR><TD> GenDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> GenDef.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> GenDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Relative </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Wildcard </TD> <TD> None </TD> </TR>
   * <TR><TD> TransformDef.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformParams.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Junit </TD> <TD> No </TD> </TR>
   * <TR><TD> NewTests </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Seed.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> DefaultTupleSize.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenOutputNew() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-new");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertEquals( "Input defs", 1, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 1, expectedTestDefs.length);

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedTestDef = new File( expectedInputDef.getParent(), "TestCases.xml");
    assertEquals( "Test def", expectedTestDef.getPath(), expectedTestDefs[0]);
    }

  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. execute (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestDef.Wildcard </TD> <TD> <FONT color="red"> Many  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenTestDefPatternInvalid() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    tcasesMojo.setTestDef( "*-Test-*.xml");

    
    // When...
    try
      {
      tcasesMojo.execute();
      fail( "Expected failure missing");
      }
    // Then...
    catch( MojoExecutionException expected)
      {
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Unexpected failure", e);
      }
    }

  /**
   * Tests {@link TcasesMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. execute (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TransformDef.Exists </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenTransformDefMissing() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    tcasesMojo.setTransformDef( "Transform.xsl");

    
    // When...
    try
      {
      tcasesMojo.execute();
      fail( "Expected failure missing");
      }
    // Then...
    catch( MojoExecutionException expected)
      {
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Unexpected failure", e);
      }
    }
  
  @Test
  public void whenProjectName() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-name");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDirFile());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDirFile());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/Other*.xml");
    assertEquals( "Input defs", 2, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 2, expectedTestDefs.length);
    }
  
  /**
   * Returns the set of paths relative to the given base directory matching any of the given patterns.
   */
  private String[] findPathsMatching( File baseDir, String... patterns)
    {
    DirectoryScanner scanner = new DirectoryScanner();
    scanner.setBasedir( baseDir);
    scanner.setIncludes( patterns);
    scanner.scan();
    return scanner.getIncludedFiles();
    }

  /**
   * Clean configured output directory.
   */
  private void clean( TcasesMojo tcasesMojo)
    {
    File outDir = tcasesMojo.getOutDirFile();
    if( outDir.exists())
      {
      try
        {
        FileUtils.cleanDirectory( outDir);
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't clear outDir=" + outDir, e);
        }
      }
    }

  /**
   * Returns the path to the base directory for the given test project.
   */
  private File getBaseDirTest( String testProjectName)
    {
    return new File( getTestProjectDir(), testProjectName);
    }

  /**
   * Returns the path to the directory containing test projects.
   */
  private File getTestProjectDir()
    {
    return new File( PlexusTestCase.getBasedir(), "src/test/resources");
    }

  @Rule
  public MojoRule mojoHelper = new MojoRule();
  }

