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
 * A {@link Matcher} for {@link SystemTestDef} objects.
 */
public class SystemTestDefMatcher implements Matcher<SystemTestDef>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, SystemTestDef expected, SystemTestDef actual)
    {
    assertEquals( label + ", name", expected.getName(), actual.getName());
    assertSetEquals( label + ", system=" + expected.getName(), expected.getFunctionTestDefs(), actual.getFunctionTestDefs(), Matchers.functionTestDefMatcher);
    }
  }
