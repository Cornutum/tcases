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
 * A composite matcher for {@link FunctionTestDef} objects.
 */
public class FunctionTestDefMatcher extends BaseCompositeMatcher<FunctionTestDef>
  {
  /**
   * Creates a new FunctionTestDefMatcher instance.
   */
  public FunctionTestDefMatcher( FunctionTestDef expected)
    {
    super( expected);

    expectThat( valueOf( "name", FunctionTestDef::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "testCases", FunctionTestDef::getTestCases).matches( visitsMembersMatching( TestCaseMatcher::new)));
    expectThat( matches( AnnotatedMatcher::new));
    }
  }
