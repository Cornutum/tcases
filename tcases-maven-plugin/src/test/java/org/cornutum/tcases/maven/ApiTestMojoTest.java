//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.maven;

import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.junit.Rule;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Arrays;
import java.util.List;

/**
 * Runs tests for the {@link ApiTestMojo} plugin.
 */
public class ApiTestMojoTest
  {
  @Test
  public void execute() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-test-project");

    // When...
    ApiTestMojo apiTestMojo = (ApiTestMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api-test");
    clean( apiTestMojo);
    apiTestMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases/openapi");
    assertThat( "Input dir", apiTestMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/generated-test-sources/java");
    assertThat( "Out dir", apiTestMojo.getOutDirFile(), is( expectedOutDir));

    List<String> expectedTests = Arrays.asList( "org/cornutum/api/MyTest.java");
    assertThat( "Tests", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTests));
    }
  
  @Test
  public void execute_default() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-test-project-default");

    // When...
    ApiTestMojo apiTestMojo = (ApiTestMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api-test");
    clean( apiTestMojo);
    apiTestMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/resources");
    assertThat( "Input dir", apiTestMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/java");
    assertThat( "Out dir", apiTestMojo.getOutDirFile(), is( expectedOutDir));

    List<String> expectedTests = Arrays.asList( "org/examples/SwaggerPetstoreTest.java");
    assertThat( "Tests", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTests));
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
  private void clean( ApiTestMojo apiMojo)
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
