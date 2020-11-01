//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.maven;

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
public class ApiTestMojoTest extends AbstractMojoTest
  {
  @Test
  public void execute() throws Exception
    {
    // Given...
    File baseDirTest = getBaseDirTest( "api-test-project");

    // When...
    ApiTestMojo apiTestMojo = (ApiTestMojo) mojoHelper.lookupConfiguredMojo( baseDirTest, "api-test");
    clean( apiTestMojo.getOutDirFile());
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
    clean( apiTestMojo.getOutDirFile());
    apiTestMojo.execute();

    // Then...
    File expectedInputDir = new File( baseDirTest, "src/test/resources");
    assertThat( "Input dir", apiTestMojo.getInputDirFile(), is( expectedInputDir));

    File expectedOutDir = new File( baseDirTest, "target/tcases/java");
    assertThat( "Out dir", apiTestMojo.getOutDirFile(), is( expectedOutDir));

    List<String> expectedTests = Arrays.asList( "org/examples/SwaggerPetstoreTest.java");
    assertThat( "Tests", Arrays.asList( findPathsMatching( expectedOutDir, "**/*")), containsMembers( expectedTests));
    }
  }
