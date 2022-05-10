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

import java.util.Map;

/**
 * Runs {@link ResponseValidator#assertHeadersValid} tests.
 */
public class ResponseHeadersValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenJsonArray()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
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
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
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
      .put( "My-Number-Array", "-123.0")
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
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Array", "")
          .put( "My-Number-Array", "A,B,C")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-Number-Array: invalid value",
          "0#items/type: Type expected 'number', found 'string'.",
          "1#items/type: Type expected 'number', found 'string'.",
          "2#items/type: Type expected 'number', found 'string'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Array", "")
          .put( "My-Number-Array", "3,9,")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-Number-Array: invalid value",
          "2#items/type: Type expected 'number', found 'string'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Array", "")
          .put( "My-Number-Array", "3,9,11")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), My-Number-Array: invalid value",
          "2#items/multipleOf: Value '11' is not a multiple of '3'.");
        });
    }

  @Test
  public void whenExplodedObject()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
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
  public void whenSimpleInteger()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "delete";
    String path = "/responses";
    int statusCode = 400;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Integer", "0")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Integer", "")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (400), My-Simple-Integer: invalid value",
          "#nullable: Null value is not allowed.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Integer", "-1,0,1")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (400), My-Simple-Integer: invalid value",
          "#type: Type expected 'integer', found 'array'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Integer", "-101")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (400), My-Simple-Integer: invalid value",
          "#minimum: Minimum is '-1', found '-101'.");
        });
    }
  
  @Test
  public void whenUnexplodedObject()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "patch";
    String path = "/responses";
    int statusCode = 400;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "A,true,B,3.12,C,Hello")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "A,false,B,,C,3.12,D,?")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "B=X,")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-Object", "C,")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Object", "C,Hello,A")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (400), My-Simple-Object: invalid value",
          "#type: Type expected 'object', found 'array'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Simple-Object", "A,")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "patch /responses (400), My-Simple-Object: invalid value",
          "A#type: Type expected 'boolean', found 'string'.");
        });
    }
  
  @Test
  public void whenSimpleString()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "get";
    String path = "/responses";
    int statusCode = 400;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-String", "123")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-String", "")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Simple-String", "A,B,C")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    }
  
  @Test
  public void whenJsonString()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "post";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Json-String", "\"Hello\"")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-String", "")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-String: Can't decode as contentType=application/json",
          "No JSON content found");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-String", "Hello")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-String: Can't decode as contentType=application/json",
          "Unrecognized token 'Hello': was expecting (JSON String, Number, Array, Object or token 'null', 'true' or 'false')",
          " at [Source: (StringReader); line: 1, column: 6]");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-String", "null")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-String: invalid value",
          "#nullable: Null value is not allowed.");
        });
    }
  
  @Test
  public void whenJsonInteger()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "post";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Json-Integer", "111222333444")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-Integer", "111222.333444")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-Integer: invalid value",
          "#type: Type expected 'integer', found 'number'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-Integer", "123/456")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-Integer: Can't decode as contentType=application/json",
          "Unexpected character ('/' (code 47)): Expected space separating root-level values",
          " at [Source: (StringReader); line: 1, column: 5]");
        });
    }
  
  @Test
  public void whenJsonObject()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "post";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Json-Object", "{ \"A\": 0 }")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    Map<String,String> headers =
      headers()
      .put( "My-Json-Object", "null")
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-Object", "{ \"A\": 0, \"B\": \"1\" }")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-Object: invalid value",
          "#maxProperties: Maximum is '1', found '2'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        Map<String,String> headers =
          headers()
          .put( "My-Json-Object", "[]")
          .build();
    
        // Then...
        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "post /responses (200), My-Json-Object: invalid value",
          "#type: Type expected 'object', found 'array'.");
        });
    }

  @Test
  public void whenForm()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", FAIL_ALL);
    String op = "put";
    String path = "/responses";
    int statusCode = 200;

    {
    // When...
    String content =
      FormUrlBuilder.with()
      .field( "O", "X,1,Y,0")
      .field( "N", "-1.234")
      .field( "S", "")
      .build();
    
    Map<String,String> headers =
      headers()
      .put( "My-Form", content)
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    String content =
      FormUrlBuilder.with()
      .field( "A", "A,B,C")
      .field( "B", "true")
      .build();
    
    Map<String,String> headers =
      headers()
      .put( "My-Form", content)
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }
    {
    // When...
    String content =
      FormUrlBuilder.with()
      .build();
    
    Map<String,String> headers =
      headers()
      .put( "My-Form", content)
      .build();
    
    // Then...
    validator.assertHeadersValid( op, path, statusCode, headers);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String content =
          FormUrlBuilder.with()
          .field( "O", "X,1,Y,0,")
          .build();
    
        Map<String,String> headers =
          headers()
          .put( "My-Form", content)
          .build();

        validator.assertHeadersValid( op, path, statusCode, headers);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "put /responses (200), My-Form: invalid value",
          "O#type: Type expected 'object', found 'array'.");
        });
    }

  @Test
  public void whenUnvalidated()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-headers", ResponseValidationHandler.FAIL_ALL);
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
