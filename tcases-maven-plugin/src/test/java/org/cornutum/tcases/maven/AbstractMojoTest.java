package org.cornutum.tcases.maven;

import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.plexus.PlexusTestCase;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.junit.Rule;
import java.io.File;

/**
 * Base class for Mojo tests.
 */
public class AbstractMojoTest
  {  
  /**
   * Returns the set of paths relative to the given base directory matching any of the given patterns.
   */
  protected String[] findPathsMatching( File baseDir, String... patterns)
    {
    DirectoryScanner scanner = new DirectoryScanner();
    scanner.setBasedir( baseDir);
    scanner.setIncludes( patterns);
    scanner.scan();
    return scanner.getIncludedFiles();
    }

  /**
   * Cleans the given output directory.
   */
  protected void clean( File outDir)
    {
    if( outDir.exists())
      {
      try
        {
        FileUtils.deleteDirectory( outDir);
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
  protected File getBaseDirTest( String testProjectName)
    {
    return new File( getTestProjectDir(), testProjectName);
    }

  /**
   * Returns the path to the directory containing test projects.
   */
  protected File getTestProjectDir()
    {
    return new File( PlexusTestCase.getBasedir(), "src/test/resources");
    }

  @Rule
  public MojoRule mojoHelper = new MojoRule();
  }

