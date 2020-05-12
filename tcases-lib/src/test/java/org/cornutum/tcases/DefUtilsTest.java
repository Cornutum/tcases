package org.cornutum.tcases;

import org.junit.Test;

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;

import java.math.BigDecimal;

import static junit.framework.TestCase.assertEquals;

public class DefUtilsTest
  {

  @Test
  public void isIdentifier()
    {
    assertTrue(DefUtils.isIdentifier("x"));
    assertTrue(DefUtils.isIdentifier("1_Bar-1"));
    assertTrue(DefUtils.isIdentifier("あ"));
    assertTrue(DefUtils.isIdentifier("Mañana_Schrödinger"));
    assertTrue(DefUtils.isIdentifier("123456789"));

    assertFalse(DefUtils.isIdentifier(null));
    assertFalse(DefUtils.isIdentifier(""));
    assertFalse(DefUtils.isIdentifier("+"));
    assertFalse(DefUtils.isIdentifier("("));
    assertFalse(DefUtils.isIdentifier("Mañana, Schrödinger"));
    assertFalse(DefUtils.isIdentifier("123456789¥"));
    }

  @Test
  public void isVarValue()
    {
    assertTrue(DefUtils.isVarValue(null));

    assertFalse(DefUtils.isVarValue("\u0007"));

    assertTrue(DefUtils.isVarValue("\n"));
    assertTrue(DefUtils.isVarValue("\t"));

    assertTrue(DefUtils.isVarValue("x"));
    assertTrue(DefUtils.isVarValue("1_Bar-1"));
    assertTrue(DefUtils.isVarValue(""));
    assertTrue(DefUtils.isVarValue(" "));
    assertTrue(DefUtils.isVarValue("+"));
    assertTrue(DefUtils.isVarValue("("));
    assertTrue(DefUtils.isVarValue("あ"));
    }

  @Test
  public void toIdentifier()
    {
    assertEquals( "1234", DefUtils.toIdentifier( 1234));
    assertEquals( "m1234", DefUtils.toIdentifier( -1234));
    assertEquals( "m1234d56", DefUtils.toIdentifier( new BigDecimal( "-1234.56")));
    assertEquals( "Is-1d2-negative", DefUtils.toIdentifier( "Is (1.2) negative?"));
    }
  }
