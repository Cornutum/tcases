//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.maven;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;

import org.apache.commons.collections4.ListUtils;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Arrays;
import java.util.List;

/**
 * Runs tests for the {@link CopyMojo} plugin.
 */
public class CopyMojoTest extends AbstractMojoTest
  {
  @Test
  public void execute() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "copy-project");

    // When...
    CopyMojo copyMojo = (CopyMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "copy");
    clean( copyMojo.getDestDirFile());
    copyMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", copyMojo.getInputDirFile(), is( expectedInputDir));

    File expectedDestDir = new File( baseDirTest, "src/test/tcases/json");
    assertThat( "Dest dir", copyMojo.getDestDirFile(), is( expectedDestDir));

    List<String> expectedCopies =
      Arrays.asList(
        "org/cornutum/tcases/examples/xml/MyCopy-Generators.json",
        "org/cornutum/tcases/examples/xml/MyCopy-Input.json",
        "org/cornutum/tcases/examples/xml/MyCopy-Test.json",
        "org/cornutum/tcases/examples/json/MyCopy-Input.json");

    assertThat( "Copied files", Arrays.asList( findPathsMatching( expectedDestDir, "**/*")), containsMembers( expectedCopies));
    }
  
  @Test
  public void execute_default() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "copy-project-default");

    // When...
    CopyMojo copyMojo = (CopyMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "copy");
    copyMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Input dir", copyMojo.getInputDirFile(), is( expectedInputDir));

    File expectedDestDir = new File( baseDirTest, "src/test/tcases");
    assertThat( "Dest dir", copyMojo.getDestDirFile(), is( expectedDestDir));

    List<String> inputFiles = Arrays.asList( findPathsMatching( expectedInputDir, "**/*"));
    List<String> copiedFiles = ListUtils.subtract( Arrays.asList( findPathsMatching( expectedDestDir, "**/*")), inputFiles);
    
    List<String> expectedCopies = Arrays.asList();
    assertThat( "Copied files", copiedFiles, containsMembers( expectedCopies));
    }
  }
