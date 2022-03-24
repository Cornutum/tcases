//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.ResponseValidator.UNVALIDATED_FAIL;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

/**
 * Runs {@link ResponseValidator#assertBodyValid} tests for string body content.
 */
public class StringBodyValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenMinLength()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-string", UNVALIDATED_FAIL);

    String op = "delete";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType;
    String bodyContent;

    // When...
    bodyContentType = "application/json";
    bodyContent = "\"123456789\"";
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String invalidContent = "\"123X\"";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, invalidContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200): invalid response",
          "Min length is '8', found '4'.",
          "'123X' does not respect pattern '^[0-9]*$'.");
        });
    }
  }
