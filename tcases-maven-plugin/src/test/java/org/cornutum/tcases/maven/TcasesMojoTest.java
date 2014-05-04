package org.cornutum.tcases.maven;

import org.cornutum.tcases.Tcases;

import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;

import org.junit.Rule;
import org.junit.Test;
import static org.junit.Assert.*;

import java.io.File;

/**
 * Runs tests for the {@link TcaseMojo} plugin.
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
    tcasesMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertEquals( "Input dir", expectedInputDir, tcasesMojo.getInputDir());

    File expectedOutDir = new File( baseDirTest, "target/tcases");
    assertEquals( "Out dir", expectedOutDir, tcasesMojo.getOutDir());

    String[] expectedInputDefs = findPathsMatching( expectedInputDir, "**/*-Input.xml");
    assertEquals( "Input defs", 1, expectedInputDefs.length);

    String[] expectedTestDefs = findPathsMatching( expectedOutDir, "**/*");
    assertEquals( "Test defs", 1, expectedTestDefs.length);

    File expectedInputDef = new File( expectedInputDefs[0]);
    File expectedTestDef = new File( expectedInputDef.getParent(), Tcases.getProjectName( expectedInputDef) + "-Test.xml");
    assertEquals( "Test def", expectedTestDef.getPath(), expectedTestDefs[0]);
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

