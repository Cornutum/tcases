//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using
 * variations of the basic properties of an API definition.
 */
public class ResponsesTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> integer </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_0()
    {
    verifyResponseInputModel( "responses-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> Other </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> simple </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> string </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_1()
    {
    verifyResponseInputModel( "responses-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> Other </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> array </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> object </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_2()
    {
    verifyResponseInputModel( "responses-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_3()
    {
    verifyResponseInputModel( "responses-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> Content-Type </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_4()
    {
    verifyResponseInputModel( "responses-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> Content-Type </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> string </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_5()
    {
    verifyResponseInputModel( "responses-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> Other </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> simple </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> integer </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> array </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_6()
    {
    verifyResponseInputModel( "responses-6");
    }
  }
