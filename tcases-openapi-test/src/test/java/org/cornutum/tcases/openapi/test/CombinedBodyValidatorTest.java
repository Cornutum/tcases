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
 * Runs {@link ResponseValidator#assertBodyValid} tests for body content matching boolean combinations of schemas.
 */
public class CombinedBodyValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenCombined()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-combined", UNVALIDATED_FAIL);

    String op = "get";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "{ \"myString\": \"12345678\" }";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    {
    // When...
    String bodyContent = "{ \"myString\": [ 1, 2, 3 ] }";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    {
    // When...
    String bodyContent = "{ \"myString\": { \"X\": 12.35, \"Y\": 8.13 } }";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"myString\": { \"X\": 12.35, \"Y\": null } }";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200): invalid response",
          "myString#oneOf/allOf/type: Type expected 'string', found 'object'.",
          "myString#oneOf/type: Type expected 'array', found 'object'.",
          "myString.Y#nullable: Null value is not allowed.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"myString\": \"ABCDEFGHI\" }";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200): invalid response",
          "myString#oneOf/allOf/maxLength: Max length is '8', found '9'.",
          "myString#oneOf/type: Type expected 'array', found 'string'.",
          "myString#oneOf/type: Type expected 'object', found 'string'.",
          "myString#oneOf/required: Field 'X' is required.",
          "myString#oneOf/required: Field 'Y' is required.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"myString\": [ \"Hello\" ] }";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200): invalid response",
          "myString#oneOf/allOf/type: Type expected 'string', found 'array'.",
          "myString#oneOf/minItems: Min items is '2', found '1'.",
          "myString.0#items/not: Schema should not be valid.",
          "myString#oneOf/type: Type expected 'object', found 'array'.",
          "myString#oneOf/required: Field 'X' is required.",
          "myString#oneOf/required: Field 'Y' is required.");
        });
    }
  }
