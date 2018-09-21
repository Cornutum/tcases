//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link SystemTestDef} objects.
 */
public class SystemTestDefMatcher extends BaseCompositeMatcher<SystemTestDef>
  {
  /**
   * Creates a new SystemTestDefMatcher instance.
   */
  public SystemTestDefMatcher( SystemTestDef expected)
    {
    super( expected);

    expectThat( valueOf( "name", SystemTestDef::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "functions", SystemTestDef::getFunctionTestDefs).matches( visitsMembersMatching( FunctionTestDefMatcher::new)));
    expectThat( matches( AnnotatedMatcher::new));
    }
  }
