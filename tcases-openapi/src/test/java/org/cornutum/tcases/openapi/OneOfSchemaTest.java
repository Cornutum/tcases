//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using
 * variations of the basic properties of an API spec.
 */
public class OneOfSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following oneOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "oneOf-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following oneOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "oneOf-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following oneOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    verifyRequestInputModel( "oneOf-2");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following oneOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    verifyRequestInputModel( "oneOf-3");
    }
  }
