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
public class AllOfSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    // properties = member

    // Given...
    //
    //   Members.Count = 1
    //
    //   Members.Type.array.Included = No
    //
    //   Members.Type.allOf.Included = Yes
    //
    //   Members.Type.boolean.Included = No
    //
    //   Members.Type.integer.Included = No
    //
    //   Members.Type.anyOf.Included = No
    //
    //   Members.Type.number.Included = No
    //
    //   Members.Type.object.Included = No
    //
    //   Members.Type.string.Included = No
    //
    //   Members.Type.oneOf.Included = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    verifyRequestInputModel( "allOf-1");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    // properties = member

    // Given...
    //
    //   Members.Count = > 1
    //
    //   Members.Type.array.Included = No
    //
    //   Members.Type.allOf.Included = No
    //
    //   Members.Type.boolean.Included = No
    //
    //   Members.Type.integer.Included = Yes
    //
    //   Members.Type.anyOf.Included = No
    //
    //   Members.Type.number.Included = Yes
    //
    //   Members.Type.object.Included = Yes
    //
    //   Members.Type.string.Included = No
    //
    //   Members.Type.oneOf.Included = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestsInputModel getRequestsInputModel} using the following allOf schema.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Members.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Members.Type.array.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.boolean.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.integer.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.number.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.object.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Members.Type.string.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Members.Type.oneOf.Included </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    // properties = member

    // Given...
    //
    //   Members.Count = > 1
    //
    //   Members.Type.array.Included = No
    //
    //   Members.Type.allOf.Included = No
    //
    //   Members.Type.boolean.Included = No
    //
    //   Members.Type.integer.Included = No
    //
    //   Members.Type.anyOf.Included = No
    //
    //   Members.Type.number.Included = No
    //
    //   Members.Type.object.Included = No
    //
    //   Members.Type.string.Included = Yes
    //
    //   Members.Type.oneOf.Included = Yes
    
    // When...

    // Then...
    }
  }
