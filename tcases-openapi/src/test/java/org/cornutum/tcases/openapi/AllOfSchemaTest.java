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
public class AllOfSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Declared </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "allOf-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Declared </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> array </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Implied </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "allOf-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Implied </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> object </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "allOf-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Declared </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "allOf-3");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Declared </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> number </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Implied </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    verifyRequestInputModel( "allOf-4");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Implied </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> string </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_5()
    {
    verifyRequestInputModel( "allOf-5");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schema (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type.Definition </TD> <TD> Declared </TD> </TR>
   * <TR><TD> Type.Value </TD> <TD> array </TD> </TR>
   * <TR><TD> Members.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Members.Type.Definition </TD> <TD> Declared </TD> </TR>
   * <TR><TD> Members.Type.Equals-Parent </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.allOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.anyOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Members.Type.Composed-With.oneOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_6()
    {
    assertRequestInputModelFailure(
      "allOf-6",
      "Error processing AllOf, /allOf, POST, param0, allOf[1]",
      "Valid types=[array] for this member are not accepted by other \"allOf\" members");

    assertRequestInputModelFailure(
      "allOf-7",
      "Error processing AllOf, /allOf, POST, param0",
      "\"allOf\" members accept types=[string] but not required types=[object]");
    }
  }
