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
public class IntegerSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int64 </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "integer-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int32 </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "integer-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "integer-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int64 </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "integer-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int32 </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    verifyRequestInputModel( "integer-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    verifyRequestInputModel( "integer-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int64 </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    verifyRequestInputModel( "integer-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int32 </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_7()
    {
    verifyRequestInputModel( "integer-7");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_8()
    {
    verifyRequestInputModel( "integer-8");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following integer schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> int64 </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_9()
    {
    verifyRequestInputModel( "integer-9");
    }
  }
