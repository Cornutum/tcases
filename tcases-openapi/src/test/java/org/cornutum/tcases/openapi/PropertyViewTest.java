//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for read-only and write-only object properties in API requests and responses.
 */
public class PropertyViewTest extends OpenApiTest
  {
  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_0()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-0");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-0", "readOnly-0-enforced");
    }
    
  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_1()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-1");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-1", "readOnly-1-enforced");
    }

  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_2()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-2");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-2", "readOnly-2-enforced");
    }

  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_3()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-3");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-3", "readOnly-3-enforced");
    }

  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_4()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-4");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-4", "readOnly-4-enforced");
    }

  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_5()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-5");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-5", "readOnly-5-enforced");
    }

  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_6()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-6");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-6", "readOnly-6-enforced");
    }

  /**
   * Tests API request input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Request (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Read-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Write-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Request_7()
    {
    // With enforcement disabled...
    verifyRequestInputModel( "readOnly-7");

    // With enforcement enabled...
    options_.setReadOnlyEnforced( true);
    verifyRequestInputModel( "readOnly-7", "readOnly-7-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_0()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-0");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-0", "writeOnly-0-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_1()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-1");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-1", "writeOnly-1-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_2()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-2");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-2", "writeOnly-2-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_3()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-3");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-3", "writeOnly-3-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_4()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-4");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-4", "writeOnly-4-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_5()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-5");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-5", "writeOnly-5-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> None </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_6()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-6");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-6", "writeOnly-6-enforced");
    }

  /**
   * Tests API response input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Response (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> properties.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> properties.Definition.Required </TD> <TD> true </TD> </TR>
   * <TR><TD> properties.Definition.Write-Only </TD> <TD> false </TD> </TR>
   * <TR><TD> properties.Read-Only </TD> <TD> Some </TD> </TR>
   * <TR><TD> minProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> maxProperties </TD> <TD> Defined </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Response_7()
    {
    // With enforcement disabled...
    verifyResponseInputModel( "writeOnly-7");

    // With enforcement enabled...
    options_.setWriteOnlyEnforced( true);
    verifyResponseInputModel( "writeOnly-7", "writeOnly-7-enforced");
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  protected ModelOptions getModelOptions()
    {
    return options_;
    }

  private ModelOptions options_ = new ModelOptions();
  }
