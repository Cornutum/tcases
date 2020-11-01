package org.cornutum.tcases.maven;

import org.apache.maven.plugin.MojoExecutionException;
import static org.cornutum.tcases.CommandUtils.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Arrays;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for the {@link AnonMojo} plugin.
 */
public class AnonMojoTest extends AbstractMojoTest
  {
  @Test
  public void withConfigDefault() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "anon-project-default");

    // When...
    AnonMojo anonMojo = (AnonMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "anon");
    clean( anonMojo.getOutDirFile());
    anonMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", anonMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", anonMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertThat( "Input defs", expectedInputDefs.length, is( 1));

    String[] expectedAnonDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Anon defs", expectedAnonDefs.length, is( 1));

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedAnonDef = new File( expectedInputDef.getParent(), String.format( "anon-%s", expectedInputDef.getName()));
    assertThat( "Anon def", expectedAnonDefs[0], is( expectedAnonDef.getPath()));
    }
  
  /**
   * Tests {@link AnonMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDefPatterns.Defined-By </TD> <TD> inputDef </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> None </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Other </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void withInputDefPatternsDefinedBy_inputDef() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "anon-project-inputDef");

    // When...
    AnonMojo anonMojo = (AnonMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "anon");
    clean( anonMojo.getOutDirFile());
    anonMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", anonMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/anonDir");
    assertThat( "Out dir", anonMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*.json");
    assertThat( "Input defs", expectedInputDefs.length, is( 0));

    assertThat( "Anon defs", expectedOutDir.exists(), is( false));
    }

  /**
   * Tests {@link AnonMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> InputDefPatterns.Defined-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Other </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> One </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void withOutFileDefined() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "anon-project-outFile");

    // When...
    AnonMojo anonMojo = (AnonMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "anon");
    clean( anonMojo.getOutDirFile());
    anonMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", anonMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/anonDir");
    assertThat( "Out dir", anonMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*.json");
    assertThat( "Input defs", expectedInputDefs.length, is( 2));

    String[] expectedAnonDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat(
      "Anon defs",
      expectedAnonDefs,
      containsElements(
        Arrays.stream( expectedInputDefs)
        .map( inputDef -> new File( inputDef))
        .map( inputDef -> new File( inputDef.getParent(), String.format( "My-%s-Anon.json", getProjectName( inputDef))))
        .map( File::getPath)
        .toArray( String[]::new)));
    }

  /**
   * Tests {@link AnonMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDefPatterns.Defined-By </TD> <TD> inputDefs </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> json </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void withInputDefPatternsDefinedBy_inputDefs() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "anon-project-inputDefs");

    // When...
    AnonMojo anonMojo = (AnonMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "anon");
    clean( anonMojo.getOutDirFile());
    anonMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", anonMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", anonMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*.input");
    assertThat( "Input defs", expectedInputDefs.length, is( 1));

    String[] expectedAnonDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat( "Anon defs", expectedAnonDefs.length, is( 1));

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedAnonDef = new File( expectedInputDef.getParent(), String.format( "anon-%s", expectedInputDef.getName()));
    assertThat( "Anon def", expectedAnonDefs[0], is( expectedAnonDef.getPath()));
    }

  /**
   * Tests {@link AnonMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDefPatterns.Defined-By </TD> <TD> project </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> xml </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> None </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void withContentTypeXml() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "anon-project-xml");

    // When...
    AnonMojo anonMojo = (AnonMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "anon");
    clean( anonMojo.getOutDirFile());
    anonMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", anonMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertThat( "Out dir", anonMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/find.*");
    assertThat( "Input defs", expectedInputDefs.length, is( 2));

    String[] expectedAnonDefs = findPathsMatching( expectedOutDir, "**/*");
    assertThat(
      "Anon defs",
      expectedAnonDefs,
      containsElements(
        Arrays.stream( expectedInputDefs)
        .map( inputDef -> new File( inputDef))
        .map( inputDef -> new File( inputDef.getParent(), "anon"))
        .map( File::getPath)
        .toArray( String[]::new)));
    }

  /**
   * Tests {@link AnonMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. execute (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> InputDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> InputDefPatterns.Defined-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OutFile.Wildcard </TD> <TD> <FONT color="red"> Many  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void whenOutFileInvalid() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "anon-project-invalid");

    // When...
    AnonMojo anonMojo = (AnonMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "anon");
    clean( anonMojo.getOutDirFile());

    expectFailure( MojoExecutionException.class)
      .when( () -> anonMojo.execute())
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat(
          "Causes",
          causes.build().collect( toList()),
          listsMembers(
            "Can't anonymize system input definition",
            "Invalid outFile pattern='*-Input.*'"));
        });
    }
  }

