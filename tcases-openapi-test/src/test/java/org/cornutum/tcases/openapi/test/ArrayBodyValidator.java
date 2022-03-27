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
 * Runs {@link ResponseValidator#assertBodyValid} tests for array body content.
 */
public class ArrayBodyValidator extends ResponseValidatorTest
  {
  @Test
  public void whenMinItems()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-array", UNVALIDATED_FAIL);

    String op = "delete";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "[\"Hello\"]";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    {
    // When...
    String bodyContent = "[\"Hello\", \"World\"]";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "[]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200): invalid response",
          "#minItems: Min items is '1', found '0'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "[\"Hello\", \"Hello\"]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200): invalid response",
          "#uniqueItems: Uniqueness is not respected 'Hello'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "[\"Hello\", null]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200): invalid response",
          "1#items/nullable: Null value is not allowed.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "\"Hello\"";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200): invalid response",
          "#type: Type expected 'array', found 'string'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "null";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200): invalid response",
          "#nullable: Null value is not allowed.");
        });
    }

  @Test
  public void whenMaxItems()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-array", UNVALIDATED_FAIL);

    String op = "get";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "[ 1, 1, 2, 3, 5]";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    {
    // When...
    String bodyContent = "[]";
    
    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    {
    // When...
    String bodyContent = "null";
    
    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "[ 1, 2, 3, 4, 5, 6]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200): invalid response",
          "#maxItems: Max items is '5', found '6'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "[ 1.23 ]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200): invalid response",
          "0#items/type: Type expected 'integer', found 'number'.");
        });
    }
  }
