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
 * variations of the basic properties of an API spec.
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
    // properties = headers,mediaTypes,op

    // Given...
    //
    //   Paths.Count = 1
    //
    //   Paths.Operations.Count = 1
    //
    //   Paths.Operations.Includes.DELETE = No
    //
    //   Paths.Operations.Includes.GET = No
    //
    //   Paths.Operations.Includes.HEAD = No
    //
    //   Paths.Operations.Includes.OPTIONS = No
    //
    //   Paths.Operations.Includes.PATCH = No
    //
    //   Paths.Operations.Includes.POST = No
    //
    //   Paths.Operations.Includes.PUT = No
    //
    //   Paths.Operations.Includes.TRACE = Yes
    //
    //   Paths.Operations.Responses.Count = 1
    //
    //   Paths.Operations.Responses.Default = Undefined
    //
    //   Paths.Operations.Responses.Response.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Count = Many
    //
    //   Paths.Operations.Responses.Response.Headers.Name = Content-Type
    //
    //   Paths.Operations.Responses.Response.Headers.Reference = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Required = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Style = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Reference = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Type = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Content.Media-Types = Many
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Reference = No
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Type = string
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. API (Success) </TH></TR>
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
    // properties = header,headers,mediaTypes,op

    // Given...
    //
    //   Paths.Count = 1
    //
    //   Paths.Operations.Count = 1
    //
    //   Paths.Operations.Includes.DELETE = Yes
    //
    //   Paths.Operations.Includes.GET = No
    //
    //   Paths.Operations.Includes.HEAD = No
    //
    //   Paths.Operations.Includes.OPTIONS = No
    //
    //   Paths.Operations.Includes.PATCH = No
    //
    //   Paths.Operations.Includes.POST = No
    //
    //   Paths.Operations.Includes.PUT = No
    //
    //   Paths.Operations.Includes.TRACE = No
    //
    //   Paths.Operations.Responses.Count = Many
    //
    //   Paths.Operations.Responses.Default = Undefined
    //
    //   Paths.Operations.Responses.Response.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Count = Many
    //
    //   Paths.Operations.Responses.Response.Headers.Name = Other
    //
    //   Paths.Operations.Responses.Response.Headers.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Required = No
    //
    //   Paths.Operations.Responses.Response.Headers.Style = simple
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Type = integer
    //
    //   Paths.Operations.Responses.Response.Content.Media-Types = 1
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Reference = No
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Type = array
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
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
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> integer </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_7()
    {
    // properties = mediaTypes,op

    // Given...
    //
    //   Paths.Count = 1
    //
    //   Paths.Operations.Count = 1
    //
    //   Paths.Operations.Includes.DELETE = No
    //
    //   Paths.Operations.Includes.GET = Yes
    //
    //   Paths.Operations.Includes.HEAD = No
    //
    //   Paths.Operations.Includes.OPTIONS = No
    //
    //   Paths.Operations.Includes.PATCH = No
    //
    //   Paths.Operations.Includes.POST = No
    //
    //   Paths.Operations.Includes.PUT = No
    //
    //   Paths.Operations.Includes.TRACE = No
    //
    //   Paths.Operations.Responses.Count = 1
    //
    //   Paths.Operations.Responses.Default = Defined
    //
    //   Paths.Operations.Responses.Response.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Count = 0
    //
    //   Paths.Operations.Responses.Response.Headers.Name = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Reference = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Required = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Style = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Reference = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Type = (not applicable)
    //
    //   Paths.Operations.Responses.Response.Content.Media-Types = Many
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Reference = No
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Type = integer
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getResponseInputModel getResponseInputModel} using the following response definitions.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.DELETE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.GET </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.HEAD </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.OPTIONS </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PATCH </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.POST </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.PUT </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Includes.TRACE </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Default </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Name </TD> <TD> Other </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Style </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Headers.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Media-Types </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Reference </TD> <TD> No </TD> </TR>
   * <TR><TD> Paths.Operations.Responses.Response.Content.Schema.Type </TD> <TD> object </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_8()
    {
    // properties = header,headers,mediaTypes,op

    // Given...
    //
    //   Paths.Count = 1
    //
    //   Paths.Operations.Count = 1
    //
    //   Paths.Operations.Includes.DELETE = No
    //
    //   Paths.Operations.Includes.GET = No
    //
    //   Paths.Operations.Includes.HEAD = Yes
    //
    //   Paths.Operations.Includes.OPTIONS = No
    //
    //   Paths.Operations.Includes.PATCH = No
    //
    //   Paths.Operations.Includes.POST = No
    //
    //   Paths.Operations.Includes.PUT = No
    //
    //   Paths.Operations.Includes.TRACE = No
    //
    //   Paths.Operations.Responses.Count = Many
    //
    //   Paths.Operations.Responses.Default = Defined
    //
    //   Paths.Operations.Responses.Response.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Count = 1
    //
    //   Paths.Operations.Responses.Response.Headers.Name = Other
    //
    //   Paths.Operations.Responses.Response.Headers.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Required = Yes
    //
    //   Paths.Operations.Responses.Response.Headers.Style = Undefined
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Reference = No
    //
    //   Paths.Operations.Responses.Response.Headers.Schema.Type = object
    //
    //   Paths.Operations.Responses.Response.Content.Media-Types = 1
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Reference = No
    //
    //   Paths.Operations.Responses.Response.Content.Schema.Type = object
    
    // When...

    // Then...
    }
  }
