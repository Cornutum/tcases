package org.cornutum.tcases.maven;

import org.apache.maven.plugin.MojoExecutionException;
import static org.cornutum.tcases.CommandUtils.*;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Arrays;

/**
 * Runs tests for the {@link TcasesMojo} plugin.
 */
public class TcasesMojoTest extends AbstractMojoTest
  {
  @Test
  public void withConfigDefault() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
    File expectedTestDef = new File( expectedInputDef.getParent(), getProjectName( expectedInputDef) + "-Test.xml");
    assertThat( "Test def", expectedTestDefs[0], is( expectedTestDef.getPath()));
    }
  @Test
  public void withJson() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-json");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
    tcasesMojo.execute();

    // Then expect XML project results...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedXmlInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertThat( "XML input defs", expectedXmlInputDefs.length, is( 1));
    File expectedXmlInputDef = new File( expectedXmlInputDefs[0]);

    String[] expectedXmlTestDefs = findPathsMatching( expectedOutDir, "**/*-Test.xml");
    assertThat( "XML test defs", expectedXmlTestDefs.length, is( 1));
    File expectedXmlTestDef = new File( expectedXmlInputDef.getParent(), getProjectName( expectedXmlInputDef) + "-Test.xml");
    assertThat( "XML test def", expectedXmlTestDefs[0], is( expectedXmlTestDef.getPath()));
    assertThat( "XML test def exists", new File( expectedOutDir, expectedXmlTestDef.getPath()).exists(), is( true));

    String[] expectedXmlGenDefs = findPathsMatching( expectedInputDir, "**/*-Generators.xml");
    assertThat( "XML generator defs", expectedXmlGenDefs.length, is( 1));
    File expectedXmlGenDef = new File( expectedXmlInputDef.getParent(), getProjectName( expectedXmlInputDef) + "-Generators.xml");
    assertThat( "XML gen def", expectedXmlGenDefs[0], is( expectedXmlGenDef.getPath()));
    assertThat( "XML gen def exists", new File( expectedInputDir, expectedXmlGenDef.getPath()).exists(), is( true));

    // And expect JSON project results...
    String[] expectedJsonInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.json");
    assertThat( "JSON input defs", expectedJsonInputDefs.length, is( 1));
    File expectedJsonInputDef = new File( expectedJsonInputDefs[0]);

    String[] expectedJsonTestDefs = findPathsMatching( expectedOutDir, "**/*-Test.json");
    assertThat( "JSON test defs", expectedJsonTestDefs.length, is( 1));
    File expectedJsonTestDef = new File( expectedJsonInputDef.getParent(), getProjectName( expectedJsonInputDef) + "-Test.json");
    assertThat( "JSON test def", expectedJsonTestDefs[0], is( expectedJsonTestDef.getPath()));
    assertThat( "JSON test def exists", new File( expectedOutDir, expectedJsonTestDef.getPath()).exists(), is( true));

    String[] expectedJsonGenDefs = findPathsMatching( expectedInputDir, "**/*-Generators.json");
    assertThat( "JSON generator defs", expectedJsonGenDefs.length, is( 1));
    File expectedJsonGenDef = new File( expectedJsonInputDef.getParent(), getProjectName( expectedJsonInputDef) + "-Generators.json");
    assertThat( "JSON gen def", expectedJsonGenDefs[0], is( expectedJsonGenDef.getPath()));
    assertThat( "JSON gen def exists", new File( expectedInputDir, expectedJsonGenDef.getPath()).exists(), is( true));
    }
  
  @Test
  public void withConfigCustom() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-custom");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
  @Test
  public void whenInputPatternsMany() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-many");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
  @Test
  public void whenOutputJUnit() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-junit");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
  @Test
  public void whenInputPatternsOne() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-one");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
  @Test
  public void whenInputDefsNone() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-none");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
  @Test
  public void whenOutputNew() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-new");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
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
  @Test
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
  @Test
  public void whenTransformDefMissing() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-default");

    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    tcasesMojo.setTransformDef( "Transform.xsl");

    expectFailure( MojoExecutionException.class)
      .when( () -> tcasesMojo.execute());
    }
  
  @Test
  public void whenProjectName() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "tcases-project-name");

    // When...
    TcasesMojo tcasesMojo = (TcasesMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "tcases");
    clean( tcasesMojo.getOutDirFile());
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", tcasesMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", tcasesMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/Other*.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 2));

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Test defs", expectedTestDefs.length, is( 2));
    }
  }

