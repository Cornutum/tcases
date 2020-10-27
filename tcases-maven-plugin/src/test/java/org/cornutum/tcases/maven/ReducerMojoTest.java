package org.cornutum.tcases.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import static org.cornutum.tcases.CommandUtils.*;

import org.junit.Rule;
import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Arrays;

/**
 * Runs tests for the {@link ReducerMojo} plugin.
 */
public class ReducerMojoTest
  {
  @Test
  public void withConfigDefault() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    // When...
    ReducerMojo reducerMojo = (ReducerMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "reduce");
    clean( baseDirTest);
    reducerMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", reducerMojo.getInputDirFile(), is( expectedInputDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 1));

    String[] expectedGenDefs = findPathsMatching( expectedInputDir, "**/*-Generators.xml");
    assertThat( "Gen defs", expectedGenDefs.length, is( 1));

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedGenDef = new File( expectedInputDef.getParent(), getProjectName( expectedInputDef) + "-Generators.xml");
    assertThat( "Gen def", expectedGenDefs[0], is( expectedGenDef.getPath()));
    }
  
  //@Test
  public void withConfigCustom() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-custom");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( baseDirTest);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "custom");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/custom");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/Input.xml", "**/InputModel");
    assertThat( "Input defs", expectedInputDefs.length, is( 2));

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Test defs", expectedTestDefs.length, is( 2));
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
  //@Test
  public void whenInputPatternsMany() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-many");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( baseDirTest);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "tcases/input");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/output");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "Input.xml", "*/The*Input.xml", "**/Other.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 3));

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Test defs", expectedTestDefs.length, is( 3));
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
  //@Test
  public void whenOutputJUnit() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-junit");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( baseDirTest);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 1));

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Test defs", expectedTestDefs.length, is( 1));

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedTestDef = new File( expectedInputDef.getParent(), getProjectName( expectedInputDef) + "Test.java");
    assertThat( "Test def", expectedTestDefs[0], is( expectedTestDef.getPath()));
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
  //@Test
  public void whenInputPatternsOne() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-one");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( baseDirTest);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/Project-*-Inputs.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 3));

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Test defs", expectedTestDefs.length, is( 3));

    Arrays.sort( expectedInputDefs);
    Arrays.sort( expectedTestDefs);
    for( int i = 0; i < expectedInputDefs.length; i++)
      {
      File expectedInputDef = new File( expectedInputDefs[i]);
      File expectedTestDef = new File( expectedInputDef.getParent(), "Tests-For-" + getProjectName( expectedInputDef) + ".xml");
      assertThat( "Test def", expectedTestDefs[i], is( expectedTestDef.getPath()));
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
  //@Test
  public void whenInputDefsNone() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-none");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( baseDirTest);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    assertThat( "Out dir created", expectedOutDir.exists(), is( false));
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
  //@Test
  public void whenOutputNew() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-new");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( baseDirTest);
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 1));

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Test defs", expectedTestDefs.length, is( 1));

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedTestDef = new File( expectedInputDef.getParent(), "TestCases.xml");
    assertThat( "Test def", expectedTestDefs[0], is( expectedTestDef.getPath()));
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
  //@Test
  public void whenTestDefPatternInvalid() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    tcasesMojo.setTestDef( "*-Test-*.xml");

    expectFailure( MojoExecutionException.class)
      .when( () -> tcasesMojo.execute());
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
  //@Test
  public void whenTransformDefMissing() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    tcasesMojo.setTransformDef( "Transform.xsl");

    expectFailure( MojoExecutionException.class)
      .when( () -> tcasesMojo.execute());
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
  private void clean( File baseDirTest)
    {
    File outDir = null;
    try
      {
      TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
      outDir = tcasesMojo.getOutDirFile();
      if( outDir.exists())
        {
        FileUtils.cleanDirectory( outDir);
        }
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't clear outDir=" + outDir, e);
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

