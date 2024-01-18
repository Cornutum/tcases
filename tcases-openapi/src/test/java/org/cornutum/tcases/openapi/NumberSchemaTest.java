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
public class NumberSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "number-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> double </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "number-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> float </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "number-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "number-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> double </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    verifyRequestInputModel( "number-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> float </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    verifyRequestInputModel( "number-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Defined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    verifyRequestInputModel( "number-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> double </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> minimum </TD> <TD> Defined </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_7()
    {
    verifyRequestInputModel( "number-7");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> float </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_8()
    {
    verifyRequestInputModel( "number-8");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following number schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
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
    verifyRequestInputModel( "number-9");
    }

  /**
   * Verifies {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} for issue 285.
   */
  @Test
  public void Schema_285()
    {
    verifyRequestInputModel( "number-285");
    }
  }
