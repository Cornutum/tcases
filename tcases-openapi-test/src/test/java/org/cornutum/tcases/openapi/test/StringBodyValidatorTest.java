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
          "#minLength: Min length is '8', found '4'.",
          "#pattern: '123X' does not respect pattern '^[0-9]*$'.");
        });
    }

  @Test
  public void whenMaxLength()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-string", UNVALIDATED_FAIL);

    String op = "get";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "\"XXXXXXXXXXXXXXXX\"";

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
        String bodyContent = "\"01234567890123456\"";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200): invalid response",
          "#maxLength: Max length is '16', found '17'.");
        });
    }

  @Test
  public void whenFormatUuid()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-string", UNVALIDATED_FAIL);

    String op = "patch";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "\"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "\"f-7-1-a-0\"";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200): invalid response",
          "#format: Value 'f-7-1-a-0' does not match format 'uuid'.");
        });
    }

  @Test
  public void whenEnum()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-string", UNVALIDATED_FAIL);

    String op = "post";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "\"Worse\"";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "\"Good\"";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200): invalid response",
          "#enum: Value 'Good' is not defined in the schema.");
        });
    }

  @Test
  public void whenFormatDate()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-string", UNVALIDATED_FAIL);

    String op = "put";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "[\"2020-12-31\"]";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "[\"2020-12-31\",\"2020/12/31\"]";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "put /responses (200): invalid response",
          "1#items/format: Value '2020/12/31' does not match format 'date'.");
        });
    }

  @Test
  public void whenFormatDateTime()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-string", UNVALIDATED_FAIL);

    String op = "trace";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/json";

    {
    // When...
    String bodyContent = "\"2019-01-10T01:02:03.456+01:01\""; 

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "\"2019-01-10:01:02:03\"";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "trace /responses (200): invalid response",
          "#format: Value '2019-01-10:01:02:03' does not match format 'date-time'.");
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
          "trace /responses (200): invalid response",
          "#nullable: Null value is not allowed.");
        });
    }
  }
