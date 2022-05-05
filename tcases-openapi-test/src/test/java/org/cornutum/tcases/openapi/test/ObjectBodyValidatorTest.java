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
 * Runs {@link ResponseValidator#assertBodyValid} tests for object body content.
 */
public class ObjectBodyValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenAdditionalPropertiesNone()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-object", FAIL_ALL);

    String op = "delete";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "{ \"myString\": \"Howdy\" }";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

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
          "#nullable: Null value is not allowed.",
          "#required: Field 'myString' is required.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200), body: invalid response",
          "#required: Field 'myString' is required.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"myString\": \"Howdy\", \"extra\": 123 }";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200), body: invalid response",
          "#additionalProperties: Additional property 'extra' is not allowed.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"myString\": 123 }";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200), body: invalid response",
          "myString#type: Type expected 'string', found 'integer'.");
        });
    }

  @Test
  public void whenAdditionalPropertiesAny()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-object", FAIL_ALL);

    String op = "get";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "{ \"myString\": \"Howdy\", \"extra\": \"Doody\"}";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }{
    // When...
    String bodyContent = "{ \"extra\": \"Doody\"}";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), body: invalid response",
          "#minProperties: Minimum is '1', found '0'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"myString\": \"Howdy\", \"myBoolean\": true, \"extra\": \"Doody\"}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), body: invalid response",
          "#maxProperties: Maximum is '2', found '3'.");
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
          "get /responses (200), body: invalid response",
          "#nullable: Null value is not allowed.");
        });
    }

  @Test
  public void whenAdditionalPropertiesDefined()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-object", FAIL_ALL);

    String op = "patch";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "{ \"myString\": \"Howdy\", \"myBoolean\": \"true\", \"extra\": \"Doody\"}";

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
        String bodyContent = "{ \"A\": \"0\", \"B\": \"1\", \"C\": \"2\", \"D\": \"3\"}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200), body: invalid response",
          "#maxProperties: Maximum is '3', found '4'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"A\": \"0\", \"B\": \"1\", \"C\": 2}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200), body: invalid response",
          "#additionalProperties/type: Type expected 'string', found 'integer'.");
        });
    }

  @Test
  public void whenWriteOnlyInvalid()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-writeOnly", FAIL_ALL);

    String op = "get";
    String path = "/object_1";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...no writeOnly property values
    String bodyContent = "{ \"A\": [], \"xyz\": { \"AP2\": \"Howdy\"}}";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    {
    // When...no writeOnly property values
    String bodyContent = "{}";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        // No writeOnly property values, but some validation errors
        String bodyContent = "{ \"A\": 1.23, \"xyz\": { \"AP2\": null}}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /object_1 (200), body: invalid response",
          "AP2#nullable: Null value is not allowed.",
          "A#type: Type expected 'array', found 'number'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        // Some writeOnly property values, and some validation errors
        String bodyContent = "{ \"A\": [{ \"id\": 1.23, \"value\": 5.67}]}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /object_1 (200), body: invalid response",
          "A.0.id#type: Type expected 'integer', found 'number'.",
          "A/0/value#writeOnly: 'writeOnly' property not allowed in response");
        });

    expectFailure( ResponseValidationException.class)
      .when( () -> {
        // Only writeOnly property value errors
        String bodyContent = "{ \"A\": [{ \"id\": 123, \"value\": 5.67}], \"xyz\": { \"AP1\": \"Hello\", \"AP2\": \"Howdy\"}}";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /object_1 (200), body: invalid response",
          "A/0/value#writeOnly: 'writeOnly' property not allowed in response",
          "xyz/AP1#writeOnly: 'writeOnly' property not allowed in response");
        });

    {
    // When...writeOnly property errors ignored
    String bodyContent = "{ \"A\": [{ \"id\": 123, \"value\": 5.67}], \"xyz\": { \"AP1\": \"Hello\", \"AP2\": \"Howdy\"}}";
    validator.writeOnlyInvalid( false);

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    }
  }
