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
 * variations of the basic properties of an API definition.
 */
public class ArraySchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> items.type </TD> <TD> number </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "array-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> items.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "array-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> items.type </TD> <TD> array </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "array-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> items.type </TD> <TD> object </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "array-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> items.type </TD> <TD> string </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    verifyRequestInputModel( "array-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> items.type </TD> <TD> boolean </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    verifyRequestInputModel( "array-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> items.type </TD> <TD> number </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    verifyRequestInputModel( "array-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following array schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxItems </TD> <TD> Defined </TD> </TR>
   * <TR><TD> uniqueItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> items.type </TD> <TD> string </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_7()
    {
    verifyRequestInputModel( "array-7");
    }
  }
