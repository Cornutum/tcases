//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.*;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.collections15.IteratorUtils;

import org.xml.sax.SAXParseException;

/**
 * Runs tests for the {@link SystemTestDocReader}.
 *
 * @version $Revision$, $Date$
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
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_0()
    {
    SystemTestDef systemTestDef = systemTestResources_.read( "system-test-def-0.xml");
    assertEquals( "System name", "System-0", systemTestDef.getName());

    FunctionTestDef[] functionTestDefs = IteratorUtils.toArray( systemTestDef.getFunctionTestDefs(), FunctionTestDef.class);
    assertEquals( "Function test defs", 3, functionTestDefs.length);
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
  public void testGetSystemTestDef_2()
    {
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
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_3()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   getSystemTestDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_4()
    {
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
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
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
   * <TR align="left"><TH colspan=2> 6.   getSystemTestDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
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
  public void testGetSystemTestDef_6()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_7()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Non-numeric </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_8()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9.   getSystemTestDef (Failure: TestCase.Id) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD><FONT color=red> Negative </FONT></TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
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
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10.  getSystemTestDef (Failure: TestCase.Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD><FONT color=red> Failure-Value-Missing </FONT></TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
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
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11.  getSystemTestDef (Failure: Input.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_11()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12.  getSystemTestDef (Failure: Var.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_12()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13.  getSystemTestDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Yes </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Undefined </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_13()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14.  getSystemTestDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Invalid </FONT></TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> No </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_14()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15.  getSystemTestDef (Failure: Var.Value) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> No </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Failure </TD> <TD> Yes </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_15()
    {
    }

  /**
   * Tests {@link SystemTestDocReader#getSystemTestDef getSystemTestDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16.  getSystemTestDef (Failure: Var.Failure) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> TestCases.System </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Count </TD> <TD> One </TD></TR>
   * <TR><TD> TestCase.Id </TD> <TD> Defined </TD></TR>
   * <TR><TD> TestCase.Failure </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Input.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Input.Type </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Value </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Failure </TD> <TD><FONT color=red> Unexpected </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemTestDef_16()
    {
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