//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using
 * variations of the basic properties of an API spec.
 */
public class NotSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> string </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> allOf </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> Same </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> anyOf </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "not-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> anyOf </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> allOf </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "not-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> object </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> oneOf </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> None </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    // properties = parentType

    // Given...
    //
    //   Parent.Type.Defined = Yes
    //
    //   Parent.Type.Value = object
    //
    //   Parent.Type.Composed-With = oneOf
    //
    //   Not.Type.Defined = No
    //
    //   Not.Type.Value = (not applicable)
    //
    //   Not.Type.Composed-With = None
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> array </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> None </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> Same </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> None </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    // properties = notType,parentType,typeNotComposable

    // Given...
    //
    //   Parent.Type.Defined = Yes
    //
    //   Parent.Type.Value = array
    //
    //   Parent.Type.Composed-With = None
    //
    //   Not.Type.Defined = Yes
    //
    //   Not.Type.Value = Same
    //
    //   Not.Type.Composed-With = None
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> number </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> allOf </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> oneOf </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    // properties = parentType

    // Given...
    //
    //   Parent.Type.Defined = Yes
    //
    //   Parent.Type.Value = number
    //
    //   Parent.Type.Composed-With = allOf
    //
    //   Not.Type.Defined = No
    //
    //   Not.Type.Value = (not applicable)
    //
    //   Not.Type.Composed-With = oneOf
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> boolean </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> None </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> Same </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> None </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    // properties = notType,parentType,typeNotComposable

    // Given...
    //
    //   Parent.Type.Defined = Yes
    //
    //   Parent.Type.Value = boolean
    //
    //   Parent.Type.Composed-With = None
    //
    //   Not.Type.Defined = Yes
    //
    //   Not.Type.Value = Same
    //
    //   Not.Type.Composed-With = None
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> string </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> anyOf </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> <FONT color="red"> Different  </FONT> </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> anyOf </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    // properties = notType,parentType

    // Given...
    //
    //   Parent.Type.Defined = Yes
    //
    //   Parent.Type.Value = string
    //
    //   Parent.Type.Composed-With = anyOf
    //
    //   Not.Type.Defined = Yes
    //
    //   Not.Type.Value = Different
    //
    //   Not.Type.Composed-With = anyOf
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schema (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parent.Type.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Parent.Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parent.Type.Composed-With </TD> <TD> anyOf </TD> </TR>
   * <TR><TD> Not.Type.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Not.Type.Value </TD> <TD> <FONT color="red"> Inapplicable  </FONT> </TD> </TR>
   * <TR><TD> Not.Type.Composed-With </TD> <TD> anyOf </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_7()
    {
    // properties = notType

    // Given...
    //
    //   Parent.Type.Defined = No
    //
    //   Parent.Type.Value = (not applicable)
    //
    //   Parent.Type.Composed-With = anyOf
    //
    //   Not.Type.Defined = Yes
    //
    //   Not.Type.Value = Inapplicable
    //
    //   Not.Type.Composed-With = anyOf
    
    // When...

    // Then...
    }
  }
