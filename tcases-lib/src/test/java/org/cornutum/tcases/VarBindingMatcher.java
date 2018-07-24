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
 * A {@link Matcher} for {@link VarBinding} objects.
 */
public class VarBindingMatcher implements Matcher<VarBinding>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, VarBinding expected, VarBinding actual)
    {
    assertEquals( label + ", var", expected.getVar(), actual.getVar());
    assertEquals( label + ", var=" + expected.getVar() + ", type", expected.getType(), actual.getType());
    assertEquals( label + ", var=" + expected.getVar() + ", value", expected.getValue(), actual.getValue());
    assertEquals( label + ", var=" + expected.getVar() + ", applicable", !expected.isValueNA(), !actual.isValueNA());
    assertEquals( label + ", var=" + expected.getVar() + ", valid", expected.isValueValid(), actual.isValueValid());
    assertMatches( label + ", var=" + expected.getVar(), expected, actual, Matchers.annotatedMatcher);
    }
  }
