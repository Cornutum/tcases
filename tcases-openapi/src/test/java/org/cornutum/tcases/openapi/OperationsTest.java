//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.SystemTestDefMatcher;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.io.SystemTestResources;
import org.cornutum.tcases.openapi.reader.OpenApiReader;
import org.cornutum.tcases.openapi.reader.OpenApiReaderException;
import static org.cornutum.tcases.util.CollectionUtils.membersOf;

import io.swagger.v3.oas.models.OpenAPI;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.InputStream;
import java.net.URL;
import java.util.Arrays;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using
 * variations of the basic properties of an API spec.
 */
public class OperationsTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_0()
    {
    verifyRequestInputModel( "operations-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Defined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> string </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> header </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> false </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> simple </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_1()
    {
    verifyRequestInputModel( "operations-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> number </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> path </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> false </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_2()
    {
    verifyRequestInputModel( "operations-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Defined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> array </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> query </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> label </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_3()
    {
    verifyRequestInputModel( "operations-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> boolean </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> cookie </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> false </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> matrix </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_4()
    {
    verifyRequestInputModel( "operations-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Defined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> object </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> header </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> form </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> false </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_5()
    {
    verifyRequestInputModel( "operations-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> path </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_6()
    {
    verifyRequestInputModel( "operations-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. API (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Defined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_7()
    {
    verifyRequestInputModel( "operations-7");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. API (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> string </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> query </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> simple </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_8()
    {
    assertOpenApiFailure( "operations-8", "attribute paths.'/operations'(post).parameters.[param0].in is missing");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. API (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> string </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> query </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> simple </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_9()
    {
    // Given...
    verifyRequestInputModelNone( "operations-9");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. API (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_10()
    {
    // Given...
    verifyRequestInputModelNone( "operations-10");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. API (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> <FONT color="red"> Undefined  </FONT> </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> string </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> query </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> simple </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_11()
    {
    assertRequestInputModelFailure( "operations-11", "API title is not defined");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. API (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> title </TD> <TD> Defined </TD> </TR>
   * <TR><TD> servers </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> paths.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> paths.operations.DELETE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.DELETE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.DELETE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.GET.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.GET.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.HEAD.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.HEAD.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.OPTIONS.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PATCH.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PATCH.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.type </TD> <TD> <FONT color="red"> None  </FONT> </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.in.Value </TD> <TD> query </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.required.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.style.Value </TD> <TD> simple </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.explode.Value </TD> <TD> true </TD> </TR>
   * <TR><TD> paths.operations.POST.parameters.content.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.POST.requestBody.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.PUT.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.PUT.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> paths.operations.TRACE.parameters.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> paths.operations.TRACE.requestBody.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void API_12()
    {
    assertRequestInputModelFailure(
      "operations-12",
      "Error processing POST /operations",
      "Error processing parameter=param0",
      "type is not defined");
    }

  /**
   * Verifies expected request input model for the given API.
   */
  private void verifyRequestInputModel( String apiName)
    {
    // Given...
    OpenAPI api = readApi( apiName);

    // When...
    SystemInputDef inputDef = TcasesOpenApi.getRequestInputModel( api);

    // Then...
    SystemInputDef expectedInputDef = readExpectedInputDef( apiName);
    assertThat( apiName + " input model", inputDef, matches( new SystemInputDefMatcher( expectedInputDef)));

    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);

    // Then...
    SystemTestDef expectedTestDef = readExpectedTestDef( apiName);
    assertThat( apiName + " test cases", testDef, matches( new SystemTestDefMatcher( expectedTestDef)));
    }

  /**
   * Verifies no request input model created for the given API.
   */
  private void verifyRequestInputModelNone( String apiName)
    {
    // Given...
    OpenAPI api = readApi( apiName);

    // When...
    SystemInputDef inputDef = TcasesOpenApi.getRequestInputModel( api);

    // Then...
    assertThat( apiName + " input model", inputDef, is( nullValue()));
    }

  /**
   * Returns the {@link OpenAPI} object represented by the given document resource.
   */
  private OpenAPI readApi( String resource)
    {
    String resourceFile = resource + ".json";
    URL url = getClass().getResource( resourceFile);
    InputStream document = getClass().getResourceAsStream( resourceFile);
    assertThat( "Resource=" + resourceFile, document, is( notNullValue()));
    
    try( OpenApiReader reader = new OpenApiReader( document, url))
      {
      return reader.read();
      }
    }

  /**
   * Returns the expected {@link SystemInputDef} object represented by the given document resource.
   */
  private SystemInputDef readExpectedInputDef( String resource)
    {
    return readInputDef( resource + "-Expected");
    }

  /**
   * Returns the {@link SystemInputDef} object represented by the given document resource.
   */
  private SystemInputDef readInputDef( String resource)
    {
    return inputResources_.read( resource + "-Input.xml");
    }

  /**
   * Returns the expected {@link SystemTestDef} object represented by the given document resource.
   */
  private SystemTestDef readExpectedTestDef( String resource)
    {
    return readTestDef( resource + "-Expected");
    }

  /**
   * Returns the {@link SystemTestDef} object represented by the given document resource.
   */
  private SystemTestDef readTestDef( String resource)
    {
    return testResources_.read( resource + "-Test.xml");
    }

  /**
   * Verifies that the given API spec contains the specified Open API conformance errors.
   */
  private void assertOpenApiFailure( String apiName, String... expected)
    {
    expectFailure( OpenApiReaderException.class)
      .when( () -> readApi( apiName))
      .then( failure -> {
        assertThat
          ( "Errors",
            membersOf( failure.getErrors()).collect( toList()),
            containsInAnyOrder( Arrays.stream( expected).map( m -> containsString(m)).collect( toList())));
        });
    }

  /**
   * Verifies that a valid input model can't be created for the given API spec for the specified reasons.
   */
  private void assertRequestInputModelFailure( String apiName, String... expected)
    {
    expectFailure( OpenApiException.class)
      .when( () -> TcasesOpenApi.getRequestInputModel( readApi( apiName)))
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat( "Causes", causes.build().collect( toList()), listsMembers( expected));
        });
    }

  private final SystemInputResources inputResources_ = new SystemInputResources( getClass());
  private final SystemTestResources testResources_ = new SystemTestResources( getClass());
  }
