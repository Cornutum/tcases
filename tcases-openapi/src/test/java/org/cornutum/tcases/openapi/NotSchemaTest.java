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
public class NotSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> array </TD> </TR>
   * <TR><TD> Composed-With.Count </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Composed-With.not.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Composed-With.allOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Composed-With.anyOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Composed-With.oneOf.Included </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_0()
    {
    verifyRequestInputModel( "not-0");
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> number </TD> </TR>
   * <TR><TD> Composed-With.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Composed-With.not.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Composed-With.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_1()
    {
    // properties = composed

    // Given...
    //
    //   Type = number
    //
    //   Composed-With.Count = 1
    //
    //   Composed-With.not.Included = Yes
    //
    //   Composed-With.allOf.Included = No
    //
    //   Composed-With.anyOf.Included = No
    //
    //   Composed-With.oneOf.Included = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> boolean </TD> </TR>
   * <TR><TD> Composed-With.Count </TD> <TD> 0 </TD> </TR>
   * <TR><TD> Composed-With.not.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_2()
    {
    // Given...
    //
    //   Type = boolean
    //
    //   Composed-With.Count = 0
    //
    //   Composed-With.not.Included = No
    //
    //   Composed-With.allOf.Included = No
    //
    //   Composed-With.anyOf.Included = No
    //
    //   Composed-With.oneOf.Included = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> object </TD> </TR>
   * <TR><TD> Composed-With.Count </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> Composed-With.not.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.allOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Composed-With.anyOf.Included </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Composed-With.oneOf.Included </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_3()
    {
    // properties = composed

    // Given...
    //
    //   Type = object
    //
    //   Composed-With.Count = > 1
    //
    //   Composed-With.not.Included = No
    //
    //   Composed-With.allOf.Included = Yes
    //
    //   Composed-With.anyOf.Included = Yes
    //
    //   Composed-With.oneOf.Included = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using the following "not" schema inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schema (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> string </TD> </TR>
   * <TR><TD> Composed-With.Count </TD> <TD> 1 </TD> </TR>
   * <TR><TD> Composed-With.not.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.allOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.anyOf.Included </TD> <TD> No </TD> </TR>
   * <TR><TD> Composed-With.oneOf.Included </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schema_4()
    {
    // properties = composed

    // Given...
    //
    //   Type = string
    //
    //   Composed-With.Count = 1
    //
    //   Composed-With.not.Included = No
    //
    //   Composed-With.allOf.Included = No
    //
    //   Composed-With.anyOf.Included = No
    //
    //   Composed-With.oneOf.Included = Yes
    
    // When...

    // Then...
    }
  }
