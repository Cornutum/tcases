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
import org.xml.sax.SAXParseException;

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
   * <TR><TD> VarSet.Members </TD> <TD> Many </TD></TR>
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
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-4.xml");
    assertEquals( "System name", "System-4", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 2, functionInputDefs.length);
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
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-5.xml");
    assertEquals( "System name", "System-5", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 1, functionInputDefs.length);
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
   * <TR><TD> VarSet.Members </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_6()
    {
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-6.xml");
    assertEquals( "System name", "System-6", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 1, functionInputDefs.length);
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
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
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
    SystemInputDef systemInputDef = readSystemInputDef( "system-input-def-7.xml");
    assertEquals( "System name", "System-7", systemInputDef.getName());

    FunctionInputDef[] functionInputDefs = IteratorUtils.toArray( systemInputDef.getFunctionInputDefs(), FunctionInputDef.class);
    assertEquals( "Function input defs", 1, functionInputDefs.length);
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
    assertException( "system-input-def-8.xml", 1, "No \"name\" attribute specified");
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
    assertException( "system-input-def-9.xml", 62, "No \"name\" attribute specified");
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
    assertException( "system-input-def-10.xml", 3, "No input variables defined for FunctionInputDef[Function-0]");
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
    assertException( "system-input-def-11.xml", 40, "No \"type\" attribute specified");
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
    assertException( "system-input-def-12.xml", 7, "No input variables defined for FunctionInputDef[Function-0]");
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
    assertException( "system-input-def-13.xml", 24, "No \"name\" attribute specified");
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
    assertException( "system-input-def-14.xml", 14, "Condition already defined for this element");
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
    assertException( "system-input-def-15.xml", 33, "Condition already defined for this element");
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
    assertException( "system-input-def-16.xml", 23, "No valid values defined for VarDef[env-0-0]");
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
    assertException( "system-input-def-17.xml", 35, "No \"name\" attribute specified");
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
    assertException( "system-input-def-18.xml", 16, "Condition already defined for this element");
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
    assertException( "system-input-def-19.xml", 32, "Condition already defined for this element");
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
    assertException( "system-input-def-20.xml", 14, "Can't define properties for a failure value");
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
    assertException( "system-input-def-21.xml", 36, "Can't define properties for a failure value");
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
    assertException( "system-input-def-22-1.xml", 10, "No property names specified");
    assertException( "system-input-def-22-2.xml", 11, "No property names specified");
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
    assertException( "system-input-def-23.xml", 19, "No \"name\" attribute specified");
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
    assertException( "system-input-def-24.xml", 39, "Condition already defined for this element");
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
    assertException( "system-input-def-25.xml", 76, "Condition already defined for this element");
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
    assertException( "system-input-def-26.xml", 11, "No members defined for VarSet[arg-0-0]");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 27.  getSystemInputDef (Failure: Condition.Members) </TH></TR>
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
  public void testGetSystemInputDef_27()
    {
    assertException( "system-input-def-27-1.xml", 22, "No member defined for this Not condition");
    assertException( "system-input-def-27-2.xml", 26, "No member defined for this When condition");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 28.  getSystemInputDef (Failure: Invalid member) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_28()
    {
    assertException( "system-input-def-28.xml", 22, "The AllOf element is not allowed at this location");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 29.  getSystemInputDef (Failure: Value.Name) </TH></TR>
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
   * <TR><TD> Value.Name </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
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
  public void testGetSystemInputDef_29()
    {
    assertException( "system-input-def-29.xml", 23, "Value=value-1 already defined for variable=member-0");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 30.  getSystemInputDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
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
  public void testGetSystemInputDef_30()
    {
    assertException( "system-input-def-30.xml", 39, "Function=Function-0 already defined for system=System-7");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 31.  getSystemInputDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
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
  public void testGetSystemInputDef_31()
    {
    assertException( "system-input-def-31.xml", 27, "Variable=env-0-0 already defined for function=Function-0");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 32.  getSystemInputDef (Failure: VarSet.Members) </TH></TR>
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
   * <TR><TD> VarSet.Members </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_32()
    {
    assertException( "system-input-def-32.xml", 26, "Member=member-0 already defined for varSet=env-0-0");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 33.  getSystemInputDef (Failure: Unknown element) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_33()
    {
    assertException( "system-input-def-33.xml", 17, "Unknown element: Vars");
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

  /**
   * Reports a failure if reading the given resource does <U>not</U> cause the expected exception at the expected location.
   */
  private void assertException( String resource, int expectedLine, String expectedMsg)
    {
    Throwable failure = null;
    try
      {
      readSystemInputDef( resource);
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

  }
