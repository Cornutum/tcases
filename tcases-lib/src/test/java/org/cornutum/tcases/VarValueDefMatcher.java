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
 * A {@link Matcher} for {@link VarValueDef} objects.
 */
public class VarValueDefMatcher implements Matcher<VarValueDef>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, VarValueDef expected, VarValueDef actual)
    {
    assertEquals( label + ", name", expected.getName(), actual.getName());
    assertEquals( label + ", value=" + expected.getName() + ", type", expected.getType(), actual.getType());
    assertEquals( label + ", value=" + expected.getName() + ", condition", expected.getCondition(), actual.getCondition());
    assertSetEquals( label + ", value=" + expected.getName() + ", properties", expected.getProperties().getProperties(), actual.getProperties().getProperties());
    assertMatches( label + ", value=" + expected.getName(), expected, actual, Matchers.annotatedMatcher);
    }
  }
