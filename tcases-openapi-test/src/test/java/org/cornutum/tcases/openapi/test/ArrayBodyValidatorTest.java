//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.ResponseValidationHandler.FAIL_ALL;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

/**
 * Runs {@link ResponseValidator#assertBodyValid} tests for array body content.
 */
public class ArrayBodyValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenMinItems()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-array", FAIL_ALL);

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
          "delete /responses (200), body: invalid response",
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
          "delete /responses (200), body: invalid response",
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
          "delete /responses (200), body: invalid response",
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
          "delete /responses (200), body: invalid response",
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
          "delete /responses (200), body: invalid response",
          "#nullable: Null value is not allowed.");
        });
    }

  @Test
  public void whenMaxItems()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-array", FAIL_ALL);

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
          "get /responses (200), body: invalid response",
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
          "get /responses (200), body: invalid response",
          "0#items/type: Type expected 'integer', found 'number'.");
        });
    }

  @Test
  public void whenWriteOnlyInvalid()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-writeOnly", FAIL_ALL);

    String op = "get";
    String path = "/array";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...no writeOnly property values
    String bodyContent = "[{\"X\": 12345}, {\"Z\": 67890}]";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    {
    // When...no writeOnly property values
    String bodyContent = "[]";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        // No writeOnly property values, but some validation errors
        String bodyContent = "[{\"X\": null}, {\"Z\": {}}]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /array (200), body: invalid response",
          "0.X#nullable: Null value is not allowed.",
          "1.Z#type: Type expected 'integer', found 'object'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        // Some writeOnly property values, and some validation errors
        String bodyContent = "[{\"X\": 12345, \"Z\": true}, {\"W0\": 34567}, {\"W2\": 56789}]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /array (200), body: invalid response",
          "0.Z#type: Type expected 'integer', found 'boolean'.",
          "1/W0#writeOnly: 'writeOnly' property not allowed in response",
          "2/W2#writeOnly: 'writeOnly' property not allowed in response");
        });

    expectFailure( ResponseValidationException.class)
      .when( () -> {
        // Only writeOnly property value errors
        String bodyContent = "[{\"W0\": 12345}, {\"W1\": 67890}, {\"W2\": 23456}]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /array (200), body: invalid response",
          "0/W0#writeOnly: 'writeOnly' property not allowed in response",
          "1/W1#writeOnly: 'writeOnly' property not allowed in response",
          "2/W2#writeOnly: 'writeOnly' property not allowed in response");
        });

    {
    // When...writeOnly property errors ignored
    String bodyContent = "[{\"W0\": 12345}, {\"W1\": 67890}, {\"W2\": 23456}]";
    validator.writeOnlyInvalid( false);

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    }
  }
