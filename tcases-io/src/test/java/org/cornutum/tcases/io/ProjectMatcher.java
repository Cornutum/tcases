//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.SystemTestDefMatcher;
import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link Project} objects.
 */
public class ProjectMatcher extends BaseCompositeMatcher<Project>
  {
  /**
   * Creates a new ProjectMatcher instance.
   */
  public ProjectMatcher( Project expected)
    {
    super( expected);

    expectThat( valueOf( "refBase", Project::getBaseLocation).matches( Matchers::equalTo));
    expectThat( valueOf( "inputDef", Project::getSystemInputValue).matches( SystemInputDefMatcher::new));
    expectThat( valueOf( "inputDefRef", Project::getSystemInputLocation).matches( Matchers::equalTo));
    expectThat( valueOf( "generators", Project::getGeneratorsValue).matches( Matchers::equalTo));
    expectThat( valueOf( "generatorsRef", Project::getGeneratorsLocation).matches( Matchers::equalTo));
    expectThat( valueOf( "baseTests", Project::getBaseTestsValue).matches( SystemTestDefMatcher::new));
    expectThat( valueOf( "baseTestsRef", Project::getBaseTestsLocation).matches( Matchers::equalTo));
    }
  }
