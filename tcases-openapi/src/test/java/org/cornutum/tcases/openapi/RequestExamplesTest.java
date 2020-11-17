//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using
 * variations of the basic properties of an API spec.
 */
public class RequestExamplesTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_0()
    {
    verifyRequestExamplesModel( "examples-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_1()
    {
    verifyRequestExamplesModel( "examples-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_2()
    {
    verifyRequestExamplesModel( "examples-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_3()
    {
    verifyRequestExamplesModel( "examples-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_4()
    {
    verifyRequestExamplesModel( "examples-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_5()
    {
    verifyRequestExamplesModel( "examples-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_6()
    {
    verifyRequestExamplesModel( "examples-6");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_7()
    {
    verifyRequestExamplesModel( "examples-7");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_8()
    {
    verifyRequestExamplesModel( "examples-8");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_9()
    {
    verifyRequestExamplesModel( "examples-9");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_10()
    {
    verifyRequestExamplesModel( "examples-10");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_11()
    {
    verifyRequestExamplesModel( "examples-11");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_12()
    {
    verifyRequestExamplesModel( "examples-12");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_13()
    {
    verifyRequestExamplesModel( "examples-13");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_14()
    {
    verifyRequestExamplesModel( "examples-14");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_15()
    {
    verifyRequestExamplesModel( "examples-15");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_16()
    {
    verifyRequestExamplesModel( "examples-16");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_17()
    {
    verifyRequestExamplesModel( "examples-17");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_18()
    {
    verifyRequestExamplesModel( "examples-18");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_19()
    {
    verifyRequestExamplesModel( "examples-19");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 20. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_20()
    {
    verifyRequestExamplesModel( "examples-20");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 21. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_21()
    {
    verifyRequestExamplesModel( "examples-21");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 22. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Enums </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> number </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_22()
    {
    verifyRequestExamplesModel( "examples-22");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 23. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Enums </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> boolean </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_23()
    {
    verifyRequestExamplesModel( "examples-23");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 24. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> integer </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_24()
    {
    verifyRequestExamplesModel( "examples-24");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 25. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> integer </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_25()
    {
    verifyRequestExamplesModel( "examples-25");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 26. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> boolean </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_26()
    {
    verifyRequestExamplesModel( "examples-26");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 27. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> array </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_27()
    {
    verifyRequestExamplesModel( "examples-27");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 28. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> number </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_28()
    {
    verifyRequestExamplesModel( "examples-28");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 29. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Enums </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> array </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_29()
    {
    verifyRequestExamplesModel( "examples-29");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 30. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> string </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_30()
    {
    verifyRequestExamplesModel( "examples-30");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 31. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_31()
    {
    verifyRequestExamplesModel( "examples-31");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 32. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> integer </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_32()
    {
    verifyRequestExamplesModel( "examples-32");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 33. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Enums </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_33()
    {
    verifyRequestExamplesModel( "examples-33");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 34. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> query </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Enums </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> string </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_34()
    {
    verifyRequestExamplesModel( "examples-34");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 35. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> cookie </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Enums </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> integer </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_35()
    {
    verifyRequestExamplesModel( "examples-35");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 36. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> path </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Property </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> array </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_36()
    {
    verifyRequestExamplesModel( "examples-36");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 37. Examples (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_37()
    {
    verifyRequestExamplesModel( "examples-37");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 38. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_38()
    {
    assertRequestExamplesModelFailure(
      "examples-38",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples",
      "Both 'anyOf' and 'oneOf' assertions defined");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 39. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_39()
    {
    assertRequestExamplesModelFailure(
      "examples-39",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples",
      "No example defined for schema of type=string");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 40. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_40()
    {
    assertRequestExamplesModelFailure(
      "examples-40",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples", 
      "Error processing examples, /examples, POST, param0, anyOf[0]",
      "Can't compose schema examples",
      "No example defined for schema of type=object");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 41. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Content </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_41()
    {
    assertRequestExamplesModelFailure(
      "examples-41",
      "Error processing examples, /examples, POST, requestBody, application/json",
      "Can't compose schema examples", 
      "No example defined for schema of type=integer");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 42. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> boolean </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_42()
    {
    assertRequestExamplesModelFailure(
      "examples-42",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples",
      "Error processing examples, /examples, POST, param0, charlie",
      "Can't compose schema examples",
      "No example defined for schema of type=string");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 43. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_43()
    {
    assertRequestExamplesModelFailure(
      "examples-43",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples",
      "If 'oneOf' defined, no other assertions allowed");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 44. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_44()
    {
    assertRequestExamplesModelFailure(
      "examples-44",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples",
      "'not' assertion defined");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestExamplesModel getRequestExamplesModel} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 45. Examples (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.In </TD> <TD> header </TD> </TR>
   * <TR><TD> Parameters.Required </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Example </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Examples </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Described-By </TD> <TD> Schema </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Example.Source </TD> <TD> Composed </TD> </TR>
   * <TR><TD> Parameters.Schema.Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Nullable </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Enum </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Leaf </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AllOf </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.AnyOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.OneOf </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Schema.Assertions.Not </TD> <TD> No </TD> </TR>
   * <TR><TD> Parameters.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Body.Required </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Example </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Examples </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Body.Content.Schema.Example.Defined </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Examples_45()
    {
    assertRequestExamplesModelFailure(
      "examples-45",
      "Error processing examples, /examples, POST, param0",
      "Can't compose schema examples",
      "'allOf' assertion defined");
    }

  @Test
  public void whenBooleanCombinations()
    {
    verifyRequestExamplesModel( "examples-combinations");
    }

  @Test
  public void whenMultipleTypes()
    {
    verifyRequestExamplesModel( "examples-multitype");
    }
  }
