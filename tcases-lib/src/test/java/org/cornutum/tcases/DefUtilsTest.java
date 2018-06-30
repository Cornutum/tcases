package org.cornutum.tcases;

import org.junit.Test;

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;

public class DefUtilsTest
  {

  @Test
  public void isIdentifier()
    {
    assertTrue(DefUtils.isIdentifier("x"));
    assertTrue(DefUtils.isIdentifier(VarValueDef.NA.getName()));
    assertTrue(DefUtils.isIdentifier("1_Bar-1"));

    assertFalse(DefUtils.isIdentifier(null));
    assertFalse(DefUtils.isIdentifier(""));
    assertFalse(DefUtils.isIdentifier("+"));
    assertFalse(DefUtils.isIdentifier("("));
    assertFalse(DefUtils.isIdentifier("あ"));
    }

  @Test
  public void isVarValue()
    {
    assertFalse(DefUtils.isVarValue(null));
    assertFalse(DefUtils.isVarValue("\u0007"));

    assertTrue(DefUtils.isVarValue("\n"));
    assertTrue(DefUtils.isVarValue("\t"));

    assertTrue(DefUtils.isVarValue("x"));
    assertTrue(DefUtils.isVarValue(VarValueDef.NA.getName()));
    assertTrue(DefUtils.isVarValue("1_Bar-1"));
    assertTrue(DefUtils.isVarValue(""));
    assertTrue(DefUtils.isVarValue(" "));
    assertTrue(DefUtils.isVarValue("+"));
    assertTrue(DefUtils.isVarValue("("));
    assertTrue(DefUtils.isVarValue("あ"));
    }
  }