package org.cornutum.tcases.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.junit.Rule;
import org.junit.Test;
import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getPath;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
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
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
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
  public void execute_1() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-1");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "**/*.api");
    assertThat( "API defs", expectedApiDefs.length, is( 1));

    List<String> expectedTestDefs = 
    Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
        String expectedApiDefDir = getPath( apiDef);
        String expectedApiDefName = getBaseName( apiDef);
        return
          Stream.of(
            String.format( "%s%sRequestsTest.java", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%sResponsesTest.java", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
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
  public void execute_2() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-2");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "**/MyApi.*");
    assertThat( "API defs", expectedApiDefs.length, is( 2));

    List<String> expectedTestDefs = 
    Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
        String expectedApiDefDir = getPath( apiDef);
        String expectedApiDefName = getBaseName( apiDef);
        return
          Stream.of(
            String.format( "%s%s-Requests-Test.json", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%s-Responses-Test.json", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
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
  public void execute_3() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-3");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "MyApi.json");
    assertThat( "API defs", expectedApiDefs.length, is( 1));

    List<String> expectedInputDefs = 
    Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
        String expectedApiDefDir = getPath( apiDef);
        String expectedApiDefName = getBaseName( apiDef);
        return
          Stream.of(
            String.format( "%s%s-Requests-Input.json", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%s-Responses-Input.json", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Input defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedInputDefs));
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
  public void execute_4() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-4");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "**/*.api", "**/*.oas");
    assertThat( "API defs", expectedApiDefs.length, is( 2));

    List<String> expectedTestDefs = 
    Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
        String expectedApiDefDir = getPath( apiDef);
        String expectedApiDefName = getBaseName( apiDef);
        return
          Stream.of(
            String.format( "%s%s-Requests-Test.json", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%s-Responses-Test.json", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
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
  public void execute_5() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-5");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "MyApi.json");
    assertThat( "API defs", expectedApiDefs.length, is( 0));
    assertThat( "Test defs", expectedOutDir.exists(), is( false));
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
  public void execute_6() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-6");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);

    expectFailure( MojoExecutionException.class)
      .when( () -> apiMojo.execute())
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
            "Can't generate requested models",
            "Error processing AllOf, /allOf, POST, param0, allOf, oneOf, oneOf[0]",
            "Ignoring this schema -- not applicable when only instance types=[string] can be valid"));
        });
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Html </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> TransformDef.Params </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformParams </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformOutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformOutFile.Params </TD> <TD> Yes </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_7() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-7");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir, "**/*");
    assertThat( "API defs", expectedApiDefs.length, is( 1));

    List<String> expectedTestDefs = 
      Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
          String expectedApiDefDir = getPath( apiDef);
          String expectedApiDefName = getBaseName( apiDef);
          return
          Stream.of(
            String.format( "%s%sRequestsTest.java", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%sResponsesTest.java", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> apiDefs </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> Many </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Html </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> TransformDef.Params </TD> <TD> No </TD> </TR>
   * <TR><TD> TransformParams </TD> <TD> No </TD> </TR>
   * <TR><TD> TransformOutFile.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> TransformOutFile.Params </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_8() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-8");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir,  "**/*.api", "**/*.oas");
    assertThat( "API defs", expectedApiDefs.length, is( 2));

    List<String> expectedTestDefs = 
      Arrays.stream( expectedApiDefs)
      .flatMap( apiDef -> {
          String expectedApiDefDir = getPath( apiDef);
          String expectedApiDefName = getBaseName( apiDef);
          return
          Stream.of(
            String.format( "%s%s-Requests-Test.json", expectedApiDefDir, expectedApiDefName),
            String.format( "%s%s-Responses-Test.json", expectedApiDefDir, expectedApiDefName));
        })
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
    }

  /**
   * Tests {@link ApiMojo#execute execute()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. execute (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> ApiDefPatterns.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> ApiDefPatterns.Defined-By </TD> <TD> project </TD> </TR>
   * <TR><TD> ApiDefPatterns.Matched </TD> <TD> One </TD> </TR>
   * <TR><TD> ContentType </TD> <TD> Default </TD> </TR>
   * <TR><TD> InputDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> OutDir </TD> <TD> Default </TD> </TR>
   * <TR><TD> Junit </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Html </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> TransformDef.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformDef.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> TransformDef.Params </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformParams </TD> <TD> No </TD> </TR>
   * <TR><TD> TransformOutFile.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> TransformOutFile.Params </TD> <TD> No </TD> </TR>
   * <TR><TD> InputModels </TD> <TD> Default </TD> </TR>
   * <TR><TD> OnCondition </TD> <TD> Default </TD> </TR>
   * <TR><TD> ReadOnlyEnforced </TD> <TD> Default </TD> </TR>
   * <TR><TD> WriteOnlyEnforced </TD> <TD> Default </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void execute_9() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-project-9");

    // When...
    ApiMojo apiMojo = (ApiMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api");
    clean( apiMojo);
    apiMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/openapi");
    assertThat( "Out dir", apiMojo.getOutDirFile(), is( expectedOutDir));

    String[] expectedApiDefs = findPathsMatching( expectedInputDir,  "**/MyApi.*");
    assertThat( "API defs", expectedApiDefs.length, is( 1));

    List<String> expectedTestDefs = 
      Arrays.stream( expectedApiDefs)
      .map( apiDef -> String.format( "%sTransformed.java", getPath( apiDef)))
      .collect( toList());
        
    assertThat( "Test defs", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTestDefs));
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
