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
 * schemas with implied types.
 */
public class ImpliedTypesTest extends OpenApiTest
  {
  @Test
  public void impliedArrays()
    {
    verifyRequestInputModel( "impliedArrays");
    }
  
  @Test
  public void impliedNumbers()
    {
    verifyRequestInputModel( "impliedNumbers");
    }
  
  @Test
  public void impliedObjects()
    {
    verifyRequestInputModel( "impliedObjects");
    }
  
  @Test
  public void impliedStrings()
    {
    verifyRequestInputModel( "impliedStrings");
    }
  
  @Test
  public void emptySchema()
    {
    verifyRequestInputModel( "emptySchema");
    }
  
  @Test
  public void impliedTypeAmbiguous()
    {
    assertRequestInputModelFailure(
      "impliedTypeAmbiguous",
      "Error processing Type, /type, POST, param0",
      "Ambiguous schema type -- could be any of array, number, object");
    }
  
  @Test
  public void impliedTypeInconsistent()
    {
    assertRequestInputModelFailure(
      "impliedTypeInconsistent",
      "Error processing Type, /type, POST, param0",
      "Schema declares type=string but has implied type=object");
    }
  }
