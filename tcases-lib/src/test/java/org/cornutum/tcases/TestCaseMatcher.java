//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.Asserts.Matcher;
import static org.cornutum.tcases.util.Asserts.*;

import static org.junit.Assert.*;

/**
 * A {@link Matcher} for {@link TestCase} objects.
 */
public class TestCaseMatcher implements Matcher<TestCase>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, TestCase expected, TestCase actual)
    {
    assertEquals( label + ", id", expected.getId(), actual.getId());
    assertSetEquals( label + ", testCase=" + expected.getId(), expected.getVarBindings(), actual.getVarBindings(), Matchers.varBindingMatcher);
    }
  }
