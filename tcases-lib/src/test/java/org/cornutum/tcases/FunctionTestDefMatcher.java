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
 * A {@link Matcher} for {@link FunctionTestDef} objects.
 */
public class FunctionTestDefMatcher implements Matcher<FunctionTestDef>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, FunctionTestDef expected, FunctionTestDef actual)
    {
    assertEquals( label + ", name", expected.getName(), actual.getName());
    assertSetEquals( label + ", function=" + expected.getName(), expected.getTestCases(), actual.getTestCases(), Matchers.testCaseMatcher);
    }
  }
