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
    // properties = bodyNone,paramObject,paramSchema,schemaComposed,schemaExample,schemaExampleComposed,schemaOneOf

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Schema
    //
    //   Parameters.Schema.Example.Defined = Yes
    //
    //   Parameters.Schema.Example.Source = Composed
    //
    //   Parameters.Schema.Type = object
    //
    //   Parameters.Schema.Assertions.Nullable = Yes
    //
    //   Parameters.Schema.Assertions.Enum = No
    //
    //   Parameters.Schema.Assertions.Leaf = No
    //
    //   Parameters.Schema.Assertions.AllOf = No
    //
    //   Parameters.Schema.Assertions.AnyOf = Invalid
    //
    //   Parameters.Schema.Assertions.OneOf = Yes
    //
    //   Parameters.Schema.Assertions.Not = No
    //
    //   Parameters.Content.Example = (not applicable)
    //
    //   Parameters.Content.Examples = (not applicable)
    //
    //   Parameters.Content.Schema.Example.Defined = (not applicable)
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
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
    // properties = bodyNone,paramContent

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Content
    //
    //   Parameters.Schema.Example.Defined = (not applicable)
    //
    //   Parameters.Schema.Example.Source = (not applicable)
    //
    //   Parameters.Schema.Type = (not applicable)
    //
    //   Parameters.Schema.Assertions.Nullable = (not applicable)
    //
    //   Parameters.Schema.Assertions.Enum = (not applicable)
    //
    //   Parameters.Schema.Assertions.Leaf = (not applicable)
    //
    //   Parameters.Schema.Assertions.AllOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.AnyOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.OneOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.Not = (not applicable)
    //
    //   Parameters.Content.Example = No
    //
    //   Parameters.Content.Examples = No
    //
    //   Parameters.Content.Schema.Example.Defined = Missing
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
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
    // properties = bodyNone,paramObject,paramSchema,schemaAnyOf,schemaComposed,schemaExample,schemaExampleComposed

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Schema
    //
    //   Parameters.Schema.Example.Defined = Yes
    //
    //   Parameters.Schema.Example.Source = Composed
    //
    //   Parameters.Schema.Type = object
    //
    //   Parameters.Schema.Assertions.Nullable = Yes
    //
    //   Parameters.Schema.Assertions.Enum = No
    //
    //   Parameters.Schema.Assertions.Leaf = No
    //
    //   Parameters.Schema.Assertions.AllOf = No
    //
    //   Parameters.Schema.Assertions.AnyOf = Yes
    //
    //   Parameters.Schema.Assertions.OneOf = Invalid
    //
    //   Parameters.Schema.Assertions.Not = No
    //
    //   Parameters.Content.Example = (not applicable)
    //
    //   Parameters.Content.Examples = (not applicable)
    //
    //   Parameters.Content.Schema.Example.Defined = (not applicable)
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
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
    // properties = paramContent,paramExample,paramExampleMany

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = Yes
    //
    //   Parameters.Described-By = Content
    //
    //   Parameters.Schema.Example.Defined = (not applicable)
    //
    //   Parameters.Schema.Example.Source = (not applicable)
    //
    //   Parameters.Schema.Type = (not applicable)
    //
    //   Parameters.Schema.Assertions.Nullable = (not applicable)
    //
    //   Parameters.Schema.Assertions.Enum = (not applicable)
    //
    //   Parameters.Schema.Assertions.Leaf = (not applicable)
    //
    //   Parameters.Schema.Assertions.AllOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.AnyOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.OneOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.Not = (not applicable)
    //
    //   Parameters.Content.Example = No
    //
    //   Parameters.Content.Examples = No
    //
    //   Parameters.Content.Schema.Example.Defined = No
    //
    //   Body.Defined = Yes
    //
    //   Body.Required = Yes
    //
    //   Body.Content.Example = No
    //
    //   Body.Content.Examples = No
    //
    //   Body.Content.Schema.Example.Defined = Missing
    
    // When...

    // Then...
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
    // properties = bodyNone,paramBoolean,paramSchema

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Schema
    //
    //   Parameters.Schema.Example.Defined = Missing
    //
    //   Parameters.Schema.Example.Source = (not applicable)
    //
    //   Parameters.Schema.Type = boolean
    //
    //   Parameters.Schema.Assertions.Nullable = (not applicable)
    //
    //   Parameters.Schema.Assertions.Enum = (not applicable)
    //
    //   Parameters.Schema.Assertions.Leaf = (not applicable)
    //
    //   Parameters.Schema.Assertions.AllOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.AnyOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.OneOf = (not applicable)
    //
    //   Parameters.Schema.Assertions.Not = (not applicable)
    //
    //   Parameters.Content.Example = (not applicable)
    //
    //   Parameters.Content.Examples = (not applicable)
    //
    //   Parameters.Content.Schema.Example.Defined = (not applicable)
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
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
    // properties = bodyNone,paramObject,paramSchema,schemaComposed,schemaExample,schemaExampleComposed,schemaOneOf

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Schema
    //
    //   Parameters.Schema.Example.Defined = Yes
    //
    //   Parameters.Schema.Example.Source = Composed
    //
    //   Parameters.Schema.Type = object
    //
    //   Parameters.Schema.Assertions.Nullable = Yes
    //
    //   Parameters.Schema.Assertions.Enum = No
    //
    //   Parameters.Schema.Assertions.Leaf = Invalid
    //
    //   Parameters.Schema.Assertions.AllOf = No
    //
    //   Parameters.Schema.Assertions.AnyOf = No
    //
    //   Parameters.Schema.Assertions.OneOf = Yes
    //
    //   Parameters.Schema.Assertions.Not = No
    //
    //   Parameters.Content.Example = (not applicable)
    //
    //   Parameters.Content.Examples = (not applicable)
    //
    //   Parameters.Content.Schema.Example.Defined = (not applicable)
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
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
    // properties = bodyNone,paramObject,paramSchema,schemaExample,schemaExampleComposed,schemaLeaf

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Schema
    //
    //   Parameters.Schema.Example.Defined = Yes
    //
    //   Parameters.Schema.Example.Source = Composed
    //
    //   Parameters.Schema.Type = object
    //
    //   Parameters.Schema.Assertions.Nullable = Yes
    //
    //   Parameters.Schema.Assertions.Enum = No
    //
    //   Parameters.Schema.Assertions.Leaf = Yes
    //
    //   Parameters.Schema.Assertions.AllOf = No
    //
    //   Parameters.Schema.Assertions.AnyOf = No
    //
    //   Parameters.Schema.Assertions.OneOf = No
    //
    //   Parameters.Schema.Assertions.Not = Invalid
    //
    //   Parameters.Content.Example = (not applicable)
    //
    //   Parameters.Content.Examples = (not applicable)
    //
    //   Parameters.Content.Schema.Example.Defined = (not applicable)
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
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
    // properties = bodyNone,paramObject,paramSchema,schemaExample,schemaExampleComposed,schemaLeaf

    // Given...
    //
    //   Parameters.Count = One
    //
    //   Parameters.In = header
    //
    //   Parameters.Required = No
    //
    //   Parameters.Example = No
    //
    //   Parameters.Examples = No
    //
    //   Parameters.Described-By = Schema
    //
    //   Parameters.Schema.Example.Defined = Yes
    //
    //   Parameters.Schema.Example.Source = Composed
    //
    //   Parameters.Schema.Type = object
    //
    //   Parameters.Schema.Assertions.Nullable = Yes
    //
    //   Parameters.Schema.Assertions.Enum = No
    //
    //   Parameters.Schema.Assertions.Leaf = Yes
    //
    //   Parameters.Schema.Assertions.AllOf = Invalid
    //
    //   Parameters.Schema.Assertions.AnyOf = No
    //
    //   Parameters.Schema.Assertions.OneOf = No
    //
    //   Parameters.Schema.Assertions.Not = No
    //
    //   Parameters.Content.Example = (not applicable)
    //
    //   Parameters.Content.Examples = (not applicable)
    //
    //   Parameters.Content.Schema.Example.Defined = (not applicable)
    //
    //   Body.Defined = No
    //
    //   Body.Required = (not applicable)
    //
    //   Body.Content.Example = (not applicable)
    //
    //   Body.Content.Examples = (not applicable)
    //
    //   Body.Content.Schema.Example.Defined = (not applicable)
    
    // When...

    // Then...
    }
  }
