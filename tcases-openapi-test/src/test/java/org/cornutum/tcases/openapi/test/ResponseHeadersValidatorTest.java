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

import java.util.Map;

/**
 * Runs {@link ResponseValidator#assertHeadersValid} tests.
 */
public class ResponseHeadersValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenContentJsonArray()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", UNVALIDATED_FAIL);
    String op = "delete";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Array", "[ 1, 2, 3 ]")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "Unexpected", "?")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Array", "null")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200), My-Array: invalid value",
          "#nullable: Null value is not allowed.");
        });
    }

  @Test
  public void whenSimpleArray()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", UNVALIDATED_FAIL);
    String op = "get";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-String", "\"A,B,C,D\"")
      .put( "My-Simple-Array", "1,2,3,")
      .put( "My-Mixed-Array", "1,,-2.0,")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Array", "")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Array", "1,2,X")
          .put( "My-Mixed-Array", "1.234")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-Simple-Array: invalid value",
          "2#items/type: Type expected 'integer', found 'string'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-String", "1.234")
          .put( "My-Simple-Array", "")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-String: invalid value",
          "#type: Type expected 'string', found 'number'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-String", "\"\"")
          .put( "My-Mixed-Array", "X,Y")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-Simple-Array: required header not received");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-String", "\"1.234\"")
          .put( "My-Simple-Array", "")
          .put( "My-Mixed-Array", "")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-Mixed-Array: invalid value",
          "#minItems: Min items is '1', found '0'.");
        });
    }

  @Test
  public void whenExplodedObject()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", UNVALIDATED_FAIL);
    String op = "patch";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "A=2014-03-21,B=0,C=")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "A=2014-03-21,B=123,C=-123")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Object", "A=,B=0,C=-123")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200), My-Simple-Object: invalid value",
          "A#format: Value '' does not match format 'date'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Object", "")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200), My-Simple-Object: invalid value",
          "#required: Field 'A' is required.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200), My-Simple-Object: required header not received");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Object", "A,B,C=")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (200), My-Simple-Object: invalid value",
          "#type: Type expected 'object', found 'array'.",
          "#required: Field 'A' is required.");
        });
    }

  @Test
  public void whenUnvalidated()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", UNVALIDATED_FAIL);
    String op = "delete";
    String path = "/unvalidated";

    // Then...
    expectFailure( IllegalArgumentException.class)
      .when( () -> {
        validator.assertHeadersValid( "GET", path, 200, headers().build());
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "GET /unvalidated: no OpenAPI response definitions found");
        });

    // Then...
    expectFailure( IllegalArgumentException.class)
      .when( () -> {
        validator.assertHeadersValid( op, "/unknown", 200, headers().build());
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /unknown: no OpenAPI response definitions found");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        validator.assertHeadersValid( op, path, 500, headers().build());
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /unvalidated: no response defined for statusCode=500");
        });

    // Then...
    expectFailure( ResponseUnvalidatedException.class)
      .when( () -> {
        int statusCode = 200;
        Map<String,String> headers =
          headers()
          .put( "X-Xml", "<Hello/>")
          .build();

        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /unvalidated (200), X-Xml: contentType=application/xml can't be validated");
        });

    // Then...
    expectFailure( ResponseUnvalidatedException.class)
      .when( () -> {
        int statusCode = 400;
        Map<String,String> headers =
          headers()
          .put( "X-Undefined-Json", "{}")
          .build();

        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /unvalidated (400), X-Undefined-Json: no schema defined");
        });

    // Then...
    expectFailure( ResponseUnvalidatedException.class)
      .when( () -> {
        int statusCode = 404;
        Map<String,String> headers =
          headers()
          .put( "X-Unknown", "?")
          .build();

        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /unvalidated (404), X-Unknown: no schema defined");
        });
    }

  /**
   * Returns a header map builder.
   */
  private MapBuilder<String,String> headers()
    {
    return new MapBuilder<String,String>();
    }

  }
