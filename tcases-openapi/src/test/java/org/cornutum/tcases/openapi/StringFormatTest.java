//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;

/**
 * Runs tests for {@link TcasesOpenApi#getRequestInputModel getRequestInputModel} using
 * variations in string format.
 */
public class StringFormatTest extends OpenApiTest
  {
  @Test
  public void whenStringFormatEmail()
    {
    // When...
    verifyRequestInputModel( "string-email");
    
    // Then...
    assertWarnings(
      "Email,/email,GET,outside: maxLength=321 exceeds maximum allowed for format=email -- using maxLength=320 instead.",
      "Email,/email,GET,outside: minLength=6 is below the minimum allowed for format=email -- using minLength=7 instead.");
    }
  
  @Test
  public void whenStringFormatUuid()
    {
    // When...
    verifyRequestInputModel( "string-uuid");
    
    // Then...
    assertWarnings(
      "Uuid,/uuid,GET,outside: maxLength=37 exceeds maximum allowed for format=uuid -- using maxLength=36 instead.",
      "Uuid,/uuid,GET,outside: minLength=35 is below the minimum allowed for format=uuid -- using minLength=36 instead.");
    }
  
  @Test
  public void whenStringFormatDate()
    {
    // When...
    verifyRequestInputModel( "string-date");
    
    // Then...
    assertWarnings(
      "Date,/date,GET,outside: maxLength=11 exceeds maximum allowed for format=date -- using maxLength=10 instead.",
      "Date,/date,GET,outside: minLength=9 is below the minimum allowed for format=date -- using minLength=10 instead.");
    }
  
  @Test
  public void whenStringFormatDateTime()
    {
    // When...
    verifyRequestInputModel( "string-date-time");
    
    // Then...
    assertWarnings(
      "Date-Time,/date-time,GET,outside: maxLength=30 exceeds maximum allowed for format=date-time -- using maxLength=29 instead.",
      "Date-Time,/date-time,GET,outside: minLength=28 is below the minimum allowed for format=date-time -- using minLength=29 instead.");
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  @Override
  protected ModelOptions getModelOptions()
    {
    return withConditionRecorder();
    }
  }
