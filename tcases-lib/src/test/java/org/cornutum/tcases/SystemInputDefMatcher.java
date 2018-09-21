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
 * A composite matcher for {@link SystemInputDef} objects.
 */
public class SystemInputDefMatcher extends BaseCompositeMatcher<SystemInputDef>
  {
  /**
   * Creates a new SystemInputDefMatcher instance.
   */
  public SystemInputDefMatcher( SystemInputDef expected)
    {
    super( expected);

    expectThat( valueOf( "name", SystemInputDef::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "functions", SystemInputDef::getFunctionInputDefs).matches( visitsMembersMatching( FunctionInputDefMatcher::new)));
    expectThat( matches( AnnotatedMatcher::new));
    }
  }
