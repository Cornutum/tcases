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
public class StringSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> Yes </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> Defined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "string-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> password </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> No </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "string-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> binary </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> Yes </TD> </TR>
   * <TR><TD> default </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "string-2");
    }
    
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> byte </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> Yes </TD> </TR>
   * <TR><TD> default </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "string-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> password </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> No </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> Defined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    verifyRequestInputModel( "string-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> Yes </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    verifyRequestInputModel( "string-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> password </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> No </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Defined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> Defined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    verifyRequestInputModel( "string-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> password </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> No </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> No </TD> </TR>
   * <TR><TD> minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> pattern </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_7()
    {
    verifyRequestInputModel( "string-7");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> uid </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> No </TD> </TR>
   * <TR><TD> default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_8()
    {
    verifyRequestInputModel( "string-8");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following string schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> format.Value </TD> <TD> password </TD> </TR>
   * <TR><TD> format.Any-Length </TD> <TD> No </TD> </TR>
   * <TR><TD> default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_9()
    {
    verifyRequestInputModel( "string-9");
    }
  
  @Test
  public void whenEmptyPathParams()
    {
    verifyRequestInputModel( "string-locations");
    }
  }
