//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.util.Asserts.*;

import org.junit.Test;
import static org.junit.Assert.*;

import org.xml.sax.SAXParseException;

/**
 * Runs tests for the {@link SystemTestDocReader}.
 *
 */
public class TestSystemTestDocReader
  {
  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_0()
    {
    // Given...
    SystemTestDef expected =
      SystemTestDefBuilder.with( "System-0")
        .functions(
          FunctionTestDefBuilder.with( "Function-0")
          .testCases(
            TestCaseBuilder.with( 0)
            .bind(
              "arg",
              VarBindingBuilder.with( "Var-00")
              .value( "Value-00")
              .build())
            .build(),
            TestCaseBuilder.with( 1)
            .bind(
              "arg",
              VarBindingBuilder.with( "Var-10")
              .value( "Value-10")
              .build())
            .build())
          .build(),
          
          FunctionTestDefBuilder.with( "Function-1")
          .testCases(
            TestCaseBuilder.with( 0)
            .bind(
              "arg",
              VarBindingBuilder.with( "Var-00")
              .value( "Value-00")
              .build())
            .build(),
            TestCaseBuilder.with( 1)
            .bind(
              "arg",
              VarBindingBuilder.with( "Var-10")
              .value( "Value-10")
              .build())
            .build())
          .build(),
          
          FunctionTestDefBuilder.with( "Function-2")
          .testCases(
            TestCaseBuilder.with( 0)
            .bind(
              "arg",
              VarBindingBuilder.with( "Var-00")
              .value( "Value-00")
              .build())
            .build(),
            TestCaseBuilder.with( 1)
            .bind(
              "arg",
              VarBindingBuilder.with( "Var-10")
              .value( "Value-10")
              .build())
            .build())
          .build())
      .build();

    // When...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-0.xml");

    // Then...
    assertMatches( "system-test-def-0.xml", expected, systemTestDef, Matchers.systemTestDefMatcher);
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_1()
    {
    // Given...
    SystemTestDef expected =
      SystemTestDefBuilder.with( "System-1")
      .build();

    // When...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-1.xml");

    // Then...
    assertMatches( "system-test-def-1.xml", expected, systemTestDef, Matchers.systemTestDefMatcher);
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_2()
    {
    // Given...
    SystemTestDef expected =
      SystemTestDefBuilder.with( "System-2")
      .functions(
        FunctionTestDefBuilder.with( "Function-0")
        .testCases(
          TestCaseBuilder.with( 0)
          .bind(
            "env",
            VarBindingBuilder.with( "VarSet-00.1.2.3")
            .value( "Value-00")
            .build(),
            VarBindingBuilder.with( "VarSet-01")
            .value( "Value-01")
            .valid( false)
            .build())
          .bind(
            "state",
            VarBindingBuilder.with( "VarSet-02")
            .value( "Value-02")
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-2.xml");

    // Then...
    assertMatches( "system-test-def-2.xml", expected, systemTestDef, Matchers.systemTestDefMatcher);
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> None </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_3()
    {
    // Given...
    SystemTestDef expected =
      SystemTestDefBuilder.with( "System-3")
        .functions(
          FunctionTestDefBuilder.with( "Function-0").build(),
          FunctionTestDefBuilder.with( "Function-1").build())
      .build();

    // When...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-3.xml");

    // Then...
    assertMatches( "system-test-def-3.xml", expected, systemTestDef, Matchers.systemTestDefMatcher);
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_4()
    {
    // Given...
    SystemTestDef expected =
      SystemTestDefBuilder.with( "System-4")
      .has( "AS00", "VS00")
      .functions(
        FunctionTestDefBuilder.with( "Function-0")
        .testCases(
          TestCaseBuilder.with( 0)
          .has( "AT00", "VT00")
          .bind(
            "arg",
            VarBindingBuilder.with( "Var-00")
            .has( "AV00", "VV00")
            .value( "Value-00")
            .build(),
            VarBindingBuilder.with( "Var-01")
            .value( "Value-01")
            .build(),
            VarBindingBuilder.with( "Var-02")
            .value( "Value-02")
            .build())
          .build(),

          TestCaseBuilder.with( 1)
          .bind(
            "arg",
            VarBindingBuilder.with( "Var-10")
            .has( "AV10", "VV10")
            .has( "AV11", "VV11")
            .has( "AV12", "VV12")
            .value( "Value-10")
            .build(),
            VarBindingBuilder.with( "Var-11")
            .value( "Value-11")
            .build(),
            VarBindingBuilder.with( "Var-12")
            .value( "Value-12")
            .build())
          .build())
        .build(),
        
        FunctionTestDefBuilder.with( "Function-1")
        .has( "AF01", "VF01")
        .testCases(
          TestCaseBuilder.with( 2)
          .bind(
            "arg",
            VarBindingBuilder.with( "Var-00")
            .value( "Value-00")
            .build(),
            VarBindingBuilder.with( "Var-01")
            .has( "AV20", "VV20")
            .value( "Value-01")
            .build(),
            VarBindingBuilder.with( "Var-02")
            .value( "Value-02")
            .build())
          .build(),

          TestCaseBuilder.with( 3)
          .has( "AT30", "VT30")
          .has( "AT31", "VT31")
          .has( "AT32", "VT32")
          .bind(
            "arg",
            VarBindingBuilder.with( "Var-10")
            .value( "Value-10")
            .build(),
            VarBindingBuilder.with( "Var-11")
            .value( "Value-11")
            .build(),
            VarBindingBuilder.with( "Var-12")
            .value( "Value-12")
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-4.xml");

    // Then...
    assertMatches( "system-test-def-4.xml", expected, systemTestDef, Matchers.systemTestDefMatcher);
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5.   getSystemTestDef (Failure: TestCases.System) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_5()
    {
    assertException( "system-test-def-5.xml", 1, "No \"system\" attribute specified");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6.   getSystemTestDef (Failure: TestCases.System) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD><FONT color=red> Invalid-Identifier </FONT></TD></TR>
   * <TR><TD> Function.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_6()
    {
    assertException( "system-test-def-6.xml", 1, "Invalid \"system\" attribute: \"System 1\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   getSystemTestDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_7()
    {
    assertException( "system-test-def-7.xml", 3, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   getSystemTestDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD><FONT color=red> Duplicate </FONT></TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> None </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> NA </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_8()
    {
    assertException( "system-test-def-8.xml", 6, "Function=Function-0 already defined for system=System-3");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9.   getSystemTestDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD><FONT color=red> Invalid-Identifier </FONT></TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_9()
    {
    assertException( "system-test-def-9.xml", 20, "Invalid \"name\" attribute: \"Function.1\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10.  getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_10()
    {
    assertException( "system-test-def-10.xml", 18, "No \"id\" attribute specified");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11.  getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Non-numeric </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_11()
    {
    assertException( "system-test-def-11.xml", 11, "Invalid \"id\" attribute: Invalid value=\"1.23\", must be a non-negative integer");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12.  getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Negative </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_12()
    {
    assertException( "system-test-def-12.xml", 11, "Invalid \"id\" attribute: Invalid value=-1, must be non-negative");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13.  getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Duplicate </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_13()
    {
    assertException( "system-test-def-13.xml", 28, "Test=1 already defined for function=Function-1");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14.  getSystemTestDef (Failure: TestCase.Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD><FONT color=red> Failure-Value-Missing </FONT></TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_14()
    {
    assertException( "system-test-def-14.xml", 10, "No failure input specified for test case=0");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15.  getSystemTestDef (Failure: Input.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_15()
    {
    assertException( "system-test-def-15.xml", 33, "No input specified for test case=0");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16.  getSystemTestDef (Failure: Var.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_16()
    {
    assertException( "system-test-def-16.xml", 9, "No input specified for test case=0");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17.  getSystemTestDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_17()
    {
    assertException( "system-test-def-17.xml", 8, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18.  getSystemTestDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Invalid </FONT></TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_18()
    {
    assertException( "system-test-def-18.xml", 12, "Invalid \"name\" attribute: \"Var 10\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19.  getSystemTestDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Duplicate </FONT></TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_19()
    {
    assertException( "system-test-def-19.xml", 10, "Binding for Var-01 already defined for testCase=0");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 20.  getSystemTestDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Invalid-Path </FONT></TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_20()
    {
    assertException( "system-test-def-20.xml", 8, "Invalid \"name\" attribute: \"Defined?\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 21.  getSystemTestDef (Failure: Var.Value) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_21()
    {
    assertException( "system-test-def-21.xml", 40, "No \"value\" attribute specified");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 22.  getSystemTestDef (Failure: Var.Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD><FONT color=red> Unexpected </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_22()
    {
    assertException( "system-test-def-22.xml", 7, "Unexpected failure value=\"Value-01\" for success test case 0");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 23.  getSystemTestDef (Failure: Invalid attribute) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_23()
    {
    assertException( "system-test-def-23.xml", 7, "Attribute=type is not allowed for Var elements");
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 24.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Non-ASCII </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_24()
    {
    // Given...
    SystemTestDef expected =
      SystemTestDefBuilder.with( "System-24")
      .functions(
        FunctionTestDefBuilder.with( "Function-0")
        .testCases(
          TestCaseBuilder.with( 0)
          .bind(
            "env",
            VarBindingBuilder.with( "Var-00.1.2.3")
            .value( "")
            .build(),
            VarBindingBuilder.with( "Var-01")
            .value( " ")
            .valid( false)
            .build())
          .bind(
            "state",
            VarBindingBuilder.with( "Var-02")
            .value( "!#")
            .build(),
            VarBindingBuilder.with( "Var-03")
            .value( "大丈夫")
            .build(),
            VarBindingBuilder.with( "Var-04")
            .value( "\t")
            .build(),
            VarBindingBuilder.with( "Var-05")
            .notApplicable()
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-24.xml");

    // Then...
    assertMatches( "system-test-def-24.xml", expected, systemTestDef, Matchers.systemTestDefMatcher);
    }

  /**
   * Reports a failure if reading the given resource does <U>not</U> cause the expected exception at the expected location.
   */
  private void assertException( String resource, int expectedLine, String expectedMsg)
    {
    Throwable failure = null;
    try
      {
      systemTestResources_.read( resource);
      }
    catch( Throwable t)
      {
      failure = t;
      }

    if( failure == null)
      {
      fail( "Expected exception not thrown.");
      }
    
    Throwable cause;
    for( cause = failure.getCause();
         !(cause == null || cause instanceof SAXParseException);
         cause = cause.getCause());

    if( cause == null)
      {
      throw new RuntimeException( "Unexpected exception thrown", failure);
      }

    SAXParseException spe = (SAXParseException) cause;
    assertEquals( "Exception line", expectedLine, spe.getLineNumber());

    String actualMsg = spe.getException()==null? spe.getMessage() : spe.getException().getMessage();
    assertEquals( "Exception message", expectedMsg, actualMsg);
    }

  private SystemTestResources systemTestResources_ = new SystemTestResources( TestSystemTestDocReader.class);
  
}
