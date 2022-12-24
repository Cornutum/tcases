//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.JsonUtils.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.joining;

/**
 * Verifies that request responses conform to the form described by an OpenAPI definition.
 */
public class ResponseValidator
  {
  /**
   * Creates a new ResponseValidator using the default {@link ResponsesDef} resource for the given test class.
   */
  public ResponseValidator( Class<?> testClass)
    {
    this( testClass, String.format( "%s-Responses.json", testClass.getSimpleName()));
    }
  
  /**
   * Creates a new ResponseValidator using the given {@link ResponsesDef} resource for the given test class.
   */
  public ResponseValidator( Class<?> testClass, String resourceName)
    {
    this( streamFor( testClass, resourceName));
    notifying( validationHandlerFor( testClass).orElse( null));
    }
  
  /**
   * Creates a new ResponseValidator using the {@link ResponsesDef} read from the given stream.
   */
  public ResponseValidator( InputStream responses)
    {
    this( readerFor( responses));
    }
  
  /**
   * Creates a new ResponseValidator using the {@link ResponsesDef} read from the given stream.
   */
  public ResponseValidator( Reader responses)
    {
    writeOnlyInvalid( writeOnlyInvalid().orElse( true));
    
    try
      {
      responses_ = ResponsesDef.read( responses);
      }
    finally
      {
      try
        {
        responses.close();
        }
      catch( Exception ignored)
        {
        // Don't care about this
        }
      }
    }

  /**
   * Changes the handler for validation condition. If <CODE>null</CODE>, the default validation
   * handler ({@link ResponseValidationHandler#EXPECT_CONFORM}) is used.
   */
  public ResponseValidator notifying( ResponseValidationHandler handler)
    {
    validationHandler_ = Optional.ofNullable( handler).orElse( ResponseValidationHandler.EXPECT_CONFORM);
    return this;
    }

  /**
   * Changes if a response containing a "writeOnly" object property is invalid. If false,
   * "writeOnly" properties are ignored during validation.
   */
  public ResponseValidator writeOnlyInvalid( boolean invalid)
    {
    writeOnlyInvalid_ = invalid;
    return this;
    }

  /**
   * Returns if a response containing a "writeOnly" object property is invalid. If false,
   * "writeOnly" properties are ignored during validation.
   */
  public boolean isWriteOnlyInvalid()
    {
    return writeOnlyInvalid_;
    }

  /**
   * Given a response with the given status code to the given operation on the API resource at the given path, completes
   * successfully if the response body conforms to its OpenAPI definition. Otherwise,
   * {@link ResponseValidationHandler#handleInvalid reports an invalid response condition}.
   */
  public void assertBodyValid( String op, String path, int statusCode, String contentType, String content)
    {
    try
      {
      if( !responses_.defined( op, path, statusCode))
        {
        throw new ResponseValidationException( op, path, String.format( "no response defined for statusCode=%s", statusCode));
        }

      // The actual body and content type must be defined if and only if an expected body content is defined.
      String bodyContentType = Objects.toString( contentType, "").trim();
      String bodyContent = Objects.toString( content, "").trim();
      boolean bodyExpected = responses_.hasBody( op, path, statusCode);
      if( bodyExpected)
        {
        if( bodyContentType.isEmpty())
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "no response Content-Type header received");
          }
        }
      else
        {
        if( !bodyContentType.isEmpty())
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "unexpected response Content-Type header received");
          }
        if( !bodyContent.isEmpty())
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "unexpected response body received");
          }
        }

      if( bodyExpected)
        {
        ContentDef bodyContentDef =
          responses_.bodyContentDef( op, path, statusCode, bodyContentType)
          .orElseThrow( () -> new ResponseValidationException( op, path, statusCode, "body", String.format( "unexpected response contentType=%s", bodyContentType)));
        
        // Compare actual body content...
        List<JsonNode> bodyContentJson = bodyContentJson( op, path, statusCode, bodyContentDef, bodyContent);
        if( bodyContentJson.isEmpty())
          {
          throw new ResponseUnvalidatedException( op, path, statusCode, "body", String.format( "contentType=%s can't be validated", bodyContentType));
          }

        // ...with expected content schema...
        JsonNode schema =
          Optional.ofNullable( bodyContentDef.getSchema())
          .orElseThrow( () -> new ResponseUnvalidatedException( op, path, statusCode, "body", String.format( "no schema defined for contentType=%s", bodyContentType)));

        // ...and report any non-conformance errors
        Optional<List<SchemaValidationError>> validationErrors;
        try
          {
          validationErrors = ResponseAnalyzer.validate( schema, bodyContentJson, isWriteOnlyInvalid());
          }
        catch( Exception e)
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "can't validate content", e);
          }

        validationErrors.ifPresent( errors -> reportValidationErrors( op, path, statusCode, "body", "invalid response", errors));
        }
      }
    catch( ResponseUnvalidatedException unvalidated)
      {
      notify( unvalidated);
      }
    catch( ResponseValidationException invalid)
      {
      notify( invalid);
      }
    }

  /**
   * Given a response with the given status code to the given operation on the API resource at the given path, completes
   * successfully if the response headers conform to its OpenAPI definition. Otherwise,
   * {@link ResponseValidationHandler#handleInvalid reports an invalid response condition}
   *
   * @param headers Maps each header name to its value
   */
  public void assertHeadersValid( String op, String path, int statusCode, Map<String,String> headers)
    {
    try
      {
      if( !responses_.defined( op, path, statusCode))
        {
        throw new ResponseValidationException( op, path, String.format( "no response defined for statusCode=%s", statusCode));
        }

      // For each header defined for this response...
      responses_.headerDefs( op, path, statusCode)
        .forEach( headerDef -> {
          // ... except Content-Type, which must be ignored...
          String headerName = headerDef.getName();
          if( !"Content-Type".equals( headerName))
            {
            if( headers.containsKey( headerName))
              {
              // ...compare expected content schema...
              ContentDef headerContentDef = headerDef.getContentDef();

              ObjectNode schema =
                Optional.ofNullable( headerContentDef.getSchema())
                .orElseThrow( () -> new ResponseUnvalidatedException( op, path, statusCode, headerName, "no schema defined"));

              // ...with actual header content...
              List<JsonNode> headerContentJson = contentJson( op, path, statusCode, headerName, headerContentDef, headers.get( headerName));
              if( headerContentJson.isEmpty())
                {
                throw new ResponseUnvalidatedException( op, path, statusCode, headerName, String.format( "contentType=%s can't be validated", headerContentDef.getContentType()));
                }

              // ...and report any non-conformance errors
              Optional<List<SchemaValidationError>> validationErrors;
              try
                {
                validationErrors = ResponseAnalyzer.validate( schema, headerContentJson, isWriteOnlyInvalid());
                }
              catch( Exception e)
                {
                throw new ResponseValidationException( op, path, statusCode, headerName, "can't validate value", e);
                }

              validationErrors.ifPresent( errors -> reportValidationErrors( op, path, statusCode, headerName, "invalid value", errors));
              }

            else if ( headerDef.isRequired())
              {
              throw new ResponseValidationException( op, path, statusCode, headerName, "required header not received");
              }
            }
          });
      }
    catch( ResponseUnvalidatedException unvalidated)
      {
      notify( unvalidated);
      }
    catch( ResponseValidationException invalid)
      {
      notify( invalid);
      }
    }

  /**
   * Returns JSON representations of the given body content. Returns {@link Optional#empty} if no JSON representation is possible.
   */
  private List<JsonNode> bodyContentJson( String op, String path, int statusCode, ContentDef contentDef, String bodyContent)
    {
    return contentJson( op, path, statusCode, "body", contentDef, bodyContent);
    }

  /**
   * Returns JSON representations of the given content. Returns {@link Collections#emptyList} if no JSON representation is possible.
   */
  private List<JsonNode> contentJson( String op, String path, int statusCode, String location, ContentDef contentDef, String content)
    {
    String contentType = contentDef.getContentType();

    try
      {
      MediaRange media = MediaRange.of( contentType);

      return
        Optional.ofNullable(
          ("application/json".equals( media.base()) || "json".equals( media.suffix()))?
          Optional.of( decodeJson( content))
          .filter( json -> json.stream().noneMatch( JsonNode::isMissingNode))
          .orElseThrow( () -> new IllegalArgumentException( "No JSON content found")):

          "text/plain".equals( media.base())?
          decodeSimple( content, contentDef) :

          "application/x-www-form-urlencoded".equals( media.base())?
          decodeFormUrl( content, contentDef) :
          
          null)

        .orElse( emptyList());
      }
    catch( Exception e)
      {
      throw new ResponseValidationException( op, path, statusCode, location, String.format( "Can't decode as contentType=%s", contentType), e);
      }
    }

  /**
   * Returns the JSON representation of JSON-encoded content.
   */
  private List<JsonNode> decodeJson( String content) throws Exception
    {
    return singletonList( readJson( content));
    }

  /**
   * Returns JSON representations of simple-encoded text content.
   */
  private List<JsonNode> decodeSimple( String content, ContentDef contentDef) throws Exception
    {
    return new SimpleDecoder( contentDef.getValueEncoding()).decode( content);
    }

  /**
   * Returns JSON representation of application/x-www-form-urlencoded content.
   */
  private List<JsonNode> decodeFormUrl( String content, ContentDef contentDef) throws Exception
    {
    return new FormUrlDecoder( contentDef).decode( content);
    }

  /**
   * Notifies a {@link ResponseUnvalidatedException} exception.
   */
  private void notify( ResponseUnvalidatedException unvalidated)
    {
    validationHandler_.handleUnvalidated( unvalidated);
    }

  /**
   * Notifies a {@link ResponseValidationException} exception.
   */
  private void notify( ResponseValidationException invalid)
    {
    validationHandler_.handleInvalid( invalid);
    }

  /**
   * Returns a message describing the errors listed in the given schema validation results.
   */
  private void reportValidationErrors( String op, String path, int statusCode, String location, String reason, List<SchemaValidationError> errors)
    {
    throw
      new ResponseValidationException
      ( op,
        path,
        statusCode,
        location,
        String.format( "%s\n%s", reason, errors.stream().map( Object::toString).collect( joining( "\n"))));
    }

  /**
   * Returns a Reader for the given stream.
   */
  private static Reader readerFor( InputStream stream)
    {
    try
      {
      return new InputStreamReader( stream, "UTF-8");
      }
    catch( Exception e)
      {
      throw new ResponseValidationException( "Can't read response definitions", e);
      }
    };

  /**
   * Returns an input stream a {@link ResponsesDef} resource for the given test class.
   */
  private static InputStream streamFor( Class<?> testClass, String resourceName)
    {
    InputStream responses;

    // Resource location specified at runtime?
    String resourceDir = Objects.toString( System.getProperty( "tcasesApiResourceDir"), "").trim();
    if( !resourceDir.isEmpty())
      {
      // Yes, read specified resource file.
      File resourceFile = new File( resourceDir, resourceName);
      try
        {
        responses = new FileInputStream( resourceFile);
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( "Can't read response definitions from " + resourceFile, e);
        }
      }
    else
      {
      // No, read resource from class path.
      responses =
        Optional.ofNullable( testClass.getResourceAsStream( resourceName))
        .orElseThrow( () -> new IllegalArgumentException( String.format( "Can't find resource=%s for class=%s", resourceName, testClass.getName())));
      }

    return responses;
    }

  /**
   * Returns the {@link ResponseValidationHandler} specified at runtime, if any.
   */
  private Optional<ResponseValidationHandler> validationHandlerFor( Class<?> testClass)
    {
    return
      // System property defines a handler class?
      Optional.of( Objects.toString( System.getProperty( "tcasesApiValidationHandler"), "").trim())
      .filter( className -> !className.isEmpty())

      .map( className -> {
        // Yes, get the fully-qualified handler class name
        String fqn =
          className.indexOf( '.') >= 0
          ? className
          : String.format( "%s.%s", testClass.getPackage().getName(), className);

        // Return an instance of the specified handler class
        ResponseValidationHandler handler;
        try
          {
          @SuppressWarnings("unchecked")
          Class<ResponseValidationHandler> handlerClass = (Class<ResponseValidationHandler>) Class.forName( fqn);

          Constructor<ResponseValidationHandler> handlerConstructor;
          if( (handlerConstructor = handlerConstructor( handlerClass, Class.class)) != null)
            {
            handler = handlerConstructor.newInstance( testClass);
            }
          else if( (handlerConstructor = handlerConstructor( handlerClass)) != null)
            {
            handler = handlerConstructor.newInstance();
            }
          else
            {
            throw new IllegalArgumentException( String.format( "Can't get constructor for class=%s", fqn));
            }
          }
        catch( Exception e)
          {
          throw new IllegalArgumentException( String.format( "Can't create instance of class=%s", fqn), e);
          }

        return handler;
        });
    }

  /**
   * Returns the {@link #writeOnlyInvalid writeOnlyInvalid() setting} specified at runtime, if any.
   */
  private Optional<Boolean> writeOnlyInvalid()
    {
    return
      Optional.of( Objects.toString( System.getProperty( "tcasesApiWriteOnlyInvalid"), "").trim())
      .filter( invalid -> !invalid.isEmpty())
      .map( invalid -> Boolean.valueOf( invalid));
    }

  /**
   * Returns a ResponseValidationHandler constructor for the given argument types.
   */
  private Constructor<ResponseValidationHandler> handlerConstructor( Class<ResponseValidationHandler> handlerClass, Class<?>... argTypes)
    {
    try
      {
      return handlerClass.getConstructor( argTypes);
      }
    catch( Exception e)
      {
      return null;
      }
    }

  @Override
  public String toString()
    {
    return
      ToString.builder( getClass())
      .add( validationHandler_)
      .addIf( "writeOnlyInvalid", Optional.of( writeOnlyInvalid_).filter( woi -> !woi))
      .toString();
    }

  private final ResponsesDef responses_;
  private ResponseValidationHandler validationHandler_ = ResponseValidationHandler.EXPECT_CONFORM;
  private boolean writeOnlyInvalid_ = true;
  }
