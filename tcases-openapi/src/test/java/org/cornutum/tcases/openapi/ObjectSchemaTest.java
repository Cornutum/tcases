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
public class ObjectSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "object-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> false </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "object-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "object-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "object-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    verifyRequestInputModel( "object-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    verifyRequestInputModel( "object-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> false </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> nullable </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    verifyRequestInputModel( "object-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_7()
    {
    verifyRequestInputModel( "object-7");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_8()
    {
    verifyRequestInputModel( "object-8");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="9">
   * <TR align="left"><TH colspan=2> 9. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_9()
    {
    verifyRequestInputModel( "object-9");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="10">
   * <TR align="left"><TH colspan=2> 10. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_10()
    {
    verifyRequestInputModel( "object-10");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="11">
   * <TR align="left"><TH colspan=2> 11. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_11()
    {
    verifyRequestInputModel( "object-11");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following object schema.
   * <P>
   * <TABLE border="1" cellpadding="11">
   * <TR align="left"><TH colspan=2> 12. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> minProperties </TD> <TD> 1 </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> properties.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> properties.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> nullable </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_12()
    {
    verifyRequestInputModel( "object-12");
    }
  
  @Test
  public void whenPropertyNonIdentifier()
    {
    verifyRequestInputModel( "object-property-names");
    }
  }
