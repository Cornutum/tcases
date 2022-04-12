//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.ResponseValidationHandler.FAIL;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

/**
 * Runs {@link ResponseValidator#assertBodyValid} tests.
 */
public class ResponseBodyValidatorTest extends ResponseValidatorTest
  {
  @Test
  public void whenBodyUndefined()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-body", FAIL);
    String op = "delete";
    String path = "/responses";
    int statusCode = 200;

    // When...
    String bodyContentType = null;
    String bodyContent = null;
    
    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    
    // When...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String contentType = "application/json";
        String content = "\"A string\"";
        validator.assertBodyValid( op, path, statusCode, contentType, content);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (200), body: unexpected response body received");
        });
    
    // When...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        int status = 404;
        String contentType = null;
        String content = "\"A string\"";
        validator.assertBodyValid( op, path, status, contentType, content);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (404), body: no response Content-Type header received");
        });
    
    // When...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        int status = 404;
        String contentType = "text/plain";
        String content = null;
        validator.assertBodyValid( op, path, status, contentType, content);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (404), body: no response body received");
        });
    }
  
  @Test
  public void whenStatusCodeUndefined()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-body", FAIL);
    String op = "delete";
    String path = "/responses";
    
    // When...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        int statusCode = 201;
        String contentType = null;
        String content = null;
        validator.assertBodyValid( op, path, statusCode, contentType, content);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses: no response defined for statusCode=201");
        });
    }
  
  @Test
  public void whenContentTypeUndefined()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-body", FAIL);
    String op = "delete";
    String path = "/responses";
    int statusCode = 400;
    String bodyContentType = "application/json";
    String bodyContent = "null";

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (400), body: unexpected response contentType=application/json");
        });
    }
  
  @Test
  public void whenContentDecodeFailed()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-body", FAIL);
    String op = "delete";
    String path = "/responses";
    int statusCode = 500;
    String bodyContentType = "application/json";

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (500), body: Can't decode as contentType=application/json",
          "No JSON content found");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String bodyContent = "{ \"key\", \"value\" }";
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "delete /responses (500), body: Can't decode as contentType=application/json",
          "Unexpected character (',' (code 44)): was expecting a colon to separate field name and value",
          " at [Source: (StringReader); line: 1, column: 9]");
        });
    }
  
  @Test
  public void whenUnvalidated()
    {
    // Given...
    String op = "get";
    String path = "/responses";

    {
    // When...
    ResponseValidator validator = validatorFor( "responsesDef-body");
    int statusCode = 200;
    String bodyContentType = "application/xml";
    String bodyContent = "<hello>world</hello>";

    // Then...
    // ResponseUnvalidatedException ignored
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    {
    // When...
    ResponseValidator validator = validatorFor( "responsesDef-body");
    int statusCode = 400;
    String bodyContentType = "application/schema+json";
    String bodyContent = "{}";

    // Then...
    // ResponseUnvalidatedException ignored
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    
    {
    // When...
    ResponseValidator validator = validatorFor( "responsesDef-body").notifying( ResponseValidationHandler.FAIL);
    int statusCode = 200;
    String bodyContentType = "application/xml";
    String bodyContent = "<hello>world</hello>";

    // Then...
    expectFailure( ResponseUnvalidatedException.class)
      .when( () -> {
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (200), body: contentType=application/xml can't be validated");
        });
    }

    {
    // When...
    ResponseValidator validator = validatorFor( "responsesDef-body").notifying( ResponseValidationHandler.FAIL);
    int statusCode = 400;
    String bodyContentType = "application/schema+json";
    String bodyContent = "{}";

    // Then...
    expectFailure( ResponseUnvalidatedException.class)
      .when( () -> {
        validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "get /responses (400), body: no schema defined for contentType=application/schema+json");
        });
    }
    }
  
  @Test
  public void whenContentTypeMatched()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-body").notifying( ResponseValidationHandler.FAIL);
    String op = "post";
    String path = "/responses";

    {
    // When...
    int statusCode = 200;
    String bodyContentType = "application/json";
    String bodyContent = "12345";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    {
    // When...
    int statusCode = 400;
    String bodyContentType = "application/schema+json";
    String bodyContent = "{}";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }

    {
    // When...
    int statusCode = 500;
    String bodyContentType = "text/plain+json";
    String bodyContent = "\"Howdy!\"";

    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, bodyContent);
    }
    }

  @Test
  public void whenForm()
    {
    // Given...
    ResponseValidator validator = validatorFor( "responsesDef-body", FAIL);
    String op = "put";
    String path = "/responses";
    int statusCode = 200;
    String bodyContentType = "application/x-www-form-urlencoded";

    {
    // When...
    String content =
      FormUrlBuilder.with()
      .field( "O", "X,1,Y,0")
      .field( "N", "-1.234")
      .field( "S", "")
      .build();
    
    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, content);
    }
    {
    // When...
    String content =
      FormUrlBuilder.with()
      .field( "A", "A,B,C")
      .field( "B", "true")
      .build();
    
    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, content);
    }
    {
    // When...
    String content =
      FormUrlBuilder.with()
      .build();
    
    // Then...
    validator.assertBodyValid( op, path, statusCode, bodyContentType, content);
    }

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String content =
          FormUrlBuilder.with()
          .field( "O", "X,1,Y,0,")
          .build();

        validator.assertBodyValid( op, path, statusCode, bodyContentType, content);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "put /responses (200), body: invalid response",
          "O#type: Type expected 'object', found 'array'.");
        });

    // Then...
    expectFailure( ResponseValidationException.class)
      .when( () -> {
        String content = "X==0";

        validator.assertBodyValid( op, path, statusCode, bodyContentType, content);
        })
      .then( failure -> {
        assertValidationErrors(
          failure,
          "put /responses (200), body: Can't decode as contentType=application/x-www-form-urlencoded",
          "'X==0' is not a valid key/value pair");
        });
    }
  }
