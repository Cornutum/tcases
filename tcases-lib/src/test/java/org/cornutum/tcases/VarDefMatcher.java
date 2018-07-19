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
 * A {@link Matcher} for {@link IVarDef} objects.
 */
public class VarDefMatcher implements Matcher<IVarDef>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, IVarDef expected, IVarDef actual)
    {
    assertEquals( label + ", name", expected.getName(), actual.getName());
    assertEquals( label + ", var=" + expected.getName() + ", type", expected.getType(), actual.getType());
    assertEquals( label + ", var=" + expected.getName() + ", condition", expected.getCondition(), actual.getCondition());
    assertSetEquals( label + ", var=" + expected.getName() + ", values", expected.getValues(), actual.getValues(), Matchers.varValueDefMatcher);
    assertSetEquals( label + ", var=" + expected.getName() + ", members", expected.getMembers(), actual.getMembers(), Matchers.varDefMatcher);
    assertMatches( label + ", var=" + expected.getName(), expected, actual, Matchers.annotatedMatcher);
    }
  }
