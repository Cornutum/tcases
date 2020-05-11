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
 * A composite matcher for {@link TestCase} objects.
 */
public class TestCaseMatcher extends BaseCompositeMatcher<TestCase>
  {
  /**
   * Creates a new TestCaseMatcher instance.
   */
  public TestCaseMatcher( TestCase expected)
    {
    super( expected);

    expectThat( valueOf( "id", TestCase::getId).matches( Matchers::equalTo));
    expectThat( valueOf( "name", TestCase::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "bindings", TestCase::getVarBindings).matches( visitsMembersMatching( VarBindingMatcher::new)));
    expectThat( matches( AnnotatedMatcher::new));
    }
  }
