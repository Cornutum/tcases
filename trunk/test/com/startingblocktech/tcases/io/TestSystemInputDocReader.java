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

import org.apache.commons.io.IOUtils;
import org.apache.commons.collections15.IteratorUtils;

import java.io.InputStream;

/**
 * Runs tests for the {@link SystemInputDocReader}.
 *
 * @version $Revision$, $Date$
 */
public class TestSystemInputDocReader
  {
  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_0()
    {
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-0.xml");
    assertEquals( "System name", "System-0", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 1, functionInputDefs.length);
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_1()
    {
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-1.xml");
    assertEquals( "System name", "System-1", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 0, functionInputDefs.length);
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_2()
    {
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-2.xml");
    assertEquals( "System name", "System-2", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 2, functionInputDefs.length);
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_3()
    {
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-3.xml");
    assertEquals( "System name", "System-3", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 2, functionInputDefs.length);
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_4()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_5()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_6()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_7()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   getSystemInputDef (Failure: System.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_8()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9.   getSystemInputDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_9()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10.  getSystemInputDef (Failure: Type.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_10()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11.  getSystemInputDef (Failure: Type.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_11()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12.  getSystemInputDef (Failure: Var.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_12()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13.  getSystemInputDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_13()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14.  getSystemInputDef (Failure: Var.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD><FONT color=red> Redefined </FONT></TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_14()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15.  getSystemInputDef (Failure: Var.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_15()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16.  getSystemInputDef (Failure: Var.ValidValues) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_16()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17.  getSystemInputDef (Failure: Value.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_17()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18.  getSystemInputDef (Failure: Value.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD><FONT color=red> Redefined </FONT></TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_18()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19.  getSystemInputDef (Failure: Value.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_19()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 20.  getSystemInputDef (Failure: Value.Properties) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD><FONT color=red> ForFailureValue </FONT></TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_20()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 21.  getSystemInputDef (Failure: Property.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD><FONT color=red> ForFailureValue </FONT></TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_21()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 22.  getSystemInputDef (Failure: Property.Properties) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_22()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 23.  getSystemInputDef (Failure: VarSet.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_23()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 24.  getSystemInputDef (Failure: VarSet.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD><FONT color=red> Redefined </FONT></TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_24()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 25.  getSystemInputDef (Failure: VarSet.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_25()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 26.  getSystemInputDef (Failure: VarSet.Members) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_26()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 27.  getSystemInputDef (Failure: VarSet.Members) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_27()
    {
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 28.  getSystemInputDef (Failure: Condition.Members) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_28()
    {
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given resource.
   */
  private SystemInputDef readSystemInputDef( String resource)
    {
    SystemInputDef  systemInputDef  = null;
    InputStream     stream          = null;
    
    try
      {
      stream = getClass().getResourceAsStream( resource);

      SystemInputDocReader reader = new SystemInputDocReader( stream);
      systemInputDef = reader.getSystemInputDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return systemInputDef;
    }

  }

