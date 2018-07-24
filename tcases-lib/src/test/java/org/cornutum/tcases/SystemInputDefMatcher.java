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
 * A {@link Matcher} for {@link SystemInputDef} objects.
 */
public class SystemInputDefMatcher implements Matcher<SystemInputDef>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, SystemInputDef expected, SystemInputDef actual)
    {
    assertEquals( label + ", name", expected.getName(), actual.getName());
    assertSetEquals( label+", system="+expected.getName()+", functions", expected.getFunctionInputDefs(), actual.getFunctionInputDefs(), Matchers.functionInputDefMatcher);
    assertMatches( label + ", system=" + expected.getName(), expected, actual, Matchers.annotatedMatcher);
    }
  }
