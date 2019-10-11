package org.cornutum.tcases.maven;

import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.junit.Rule;
import org.junit.Test;
import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getPath;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for the {@link ApiMojo} plugin.
 */
public class ApiMojoTest
  {
  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Other </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Other </TD> </TR>
   * <TR><TD> Junit </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Html </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_0() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-0");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/other");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/other");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "**/*.yaml", "**/*.json");
    assertThat( "API defs", expectedApiDefs.length, is( 2));

    List<String> expectedTestDefs = 
    Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
        String expectedApiDefDir = getPath( apiDef);
        String expectedApiDefName = getBaseName( apiDef);
        return
          Stream.of(
            String.format( "%s%s-Requests-Test.htm", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%s-Responses-Test.htm", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*.htm")), containsMembers( expectedTestDefs));
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> apiDefs </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> yaml </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Html </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> No </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Yes </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_1()
    {
    // properties = apiDef,apiDefs,inputPatterns,junit

    // Given...
    //
    //   ApiDefPatterns.Count = Many
    //
    //   ApiDefPatterns.Defined-By = apiDefs
    //
    //   ApiDefPatterns.Matched = One
    //
    //   ContentType = yaml
    //
    //   InputDir = Default
    //
    //   OutDir = Default
    //
    //   Junit = Yes
    //
    //   Html = (not applicable)
    //
    //   InputModels = No
    //
    //   OnCondition = Default
    //
    //   ReadOnlyEnforced = Yes
    //
    //   WriteOnlyEnforced = Default
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> project </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> No </TD> </TR>
   * <TR><TD> Html </TD> <TD> Default </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> No </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_2()
    {
    // properties = apiDef,inputPatterns

    // Given...
    //
    //   ApiDefPatterns.Count = One
    //
    //   ApiDefPatterns.Defined-By = project
    //
    //   ApiDefPatterns.Matched = Many
    //
    //   ContentType = Default
    //
    //   InputDir = Default
    //
    //   OutDir = Default
    //
    //   Junit = No
    //
    //   Html = Default
    //
    //   InputModels = Default
    //
    //   OnCondition = Default
    //
    //   ReadOnlyEnforced = No
    //
    //   WriteOnlyEnforced = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Html </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Yes </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_3()
    {
    // properties = apiDef,inputModels

    // Given...
    //
    //   ApiDefPatterns.Count = None
    //
    //   ApiDefPatterns.Defined-By = (not applicable)
    //
    //   ApiDefPatterns.Matched = One
    //
    //   ContentType = (not applicable)
    //
    //   InputDir = Default
    //
    //   OutDir = Default
    //
    //   Junit = (not applicable)
    //
    //   Html = (not applicable)
    //
    //   InputModels = Yes
    //
    //   OnCondition = Default
    //
    //   ReadOnlyEnforced = Default
    //
    //   WriteOnlyEnforced = Default
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> apiDefs </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> Default </TD> </TR>
   * <TR><TD> Html </TD> <TD> No </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_4()
    {
    // properties = apiDef,apiDefs,inputPatterns

    // Given...
    //
    //   ApiDefPatterns.Count = Many
    //
    //   ApiDefPatterns.Defined-By = apiDefs
    //
    //   ApiDefPatterns.Matched = Many
    //
    //   ContentType = Default
    //
    //   InputDir = Default
    //
    //   OutDir = Default
    //
    //   Junit = Default
    //
    //   Html = No
    //
    //   InputModels = Default
    //
    //   OnCondition = Default
    //
    //   ReadOnlyEnforced = Default
    //
    //   WriteOnlyEnforced = Default
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> apiDef </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> None </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Junit </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Html </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_5()
    {
    // properties = inputPatterns

    // Given...
    //
    //   ApiDefPatterns.Count = One
    //
    //   ApiDefPatterns.Defined-By = apiDef
    //
    //   ApiDefPatterns.Matched = None
    //
    //   ContentType = (not applicable)
    //
    //   InputDir = Default
    //
    //   OutDir = (not applicable)
    //
    //   Junit = (not applicable)
    //
    //   Html = (not applicable)
    //
    //   InputModels = (not applicable)
    //
    //   OnCondition = (not applicable)
    //
    //   ReadOnlyEnforced = (not applicable)
    //
    //   WriteOnlyEnforced = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. execute (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> Default </TD> </TR>
   * <TR><TD> Html </TD> <TD> Default </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> <FONT color="red"> fail  </FONT> </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_6()
    {
    // properties = apiDef

    // Given...
    //
    //   ApiDefPatterns.Count = None
    //
    //   ApiDefPatterns.Defined-By = (not applicable)
    //
    //   ApiDefPatterns.Matched = One
    //
    //   ContentType = (not applicable)
    //
    //   InputDir = Default
    //
    //   OutDir = Default
    //
    //   Junit = Default
    //
    //   Html = Default
    //
    //   InputModels = Default
    //
    //   OnCondition = fail
    //
    //   ReadOnlyEnforced = Default
    //
    //   WriteOnlyEnforced = Default
    
    // When...

    // Then...
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
  private void clean( ApiMojo apiMojo)
    {
    File outDir = apiMojo.getOutDirFile();
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
