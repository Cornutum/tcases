//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.openapi4j.core.validation.ValidationResults.ValidationItem;
import org.openapi4j.core.validation.ValidationSeverity;
import org.openapi4j.schema.validator.ValidationData;
import org.openapi4j.schema.validator.v3.SchemaValidator;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.function.Consumer;

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
    this( testClass.getResourceAsStream( resourceName));
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
        }
      }
    }

  /**
   * Changes the handler for {@link ResponseUnvalidatedException} actions. If <CODE>null</CODE>,
   * {@link ResponseUnvalidatedException} actions are ignored (the default).
   */
  public ResponseValidator whenUnvalidated( Consumer<ResponseUnvalidatedException> handler)
    {
    unvalidatedHandler_ = Optional.ofNullable( handler).orElse( UNVALIDATED_IGNORE);
    return this;
    }

  /**
   * Given a response with the given status code to the given operation on the API resource at the given path, completes
   * successfully if the response body conforms to its OpenAPI definition. Otherwise, throws a ResponseValidationException.
   */
  public void assertBodyValid( String op, String path, int statusCode, String contentType, String bodyContent) throws ResponseValidationException
    {
    try
      {
      if( !responses_.defined( op, path, statusCode))
        {
        throw new ResponseValidationException( op, path, String.format( "no response defined for statusCode=%s", statusCode));
        }

      // The actual body and content type must be defined if and only if an expected body content is defined.
      if( responses_.hasBody( op, path, statusCode))
        {
        if( contentType == null)
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "no response Content-Type header received");
          }
        if( bodyContent == null)
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "no response body received");
          }
        }
      else if( bodyContent != null)
        {
        throw new ResponseValidationException( op, path, statusCode, "body", "unexpected response body received");
        }

      if( bodyContent != null)
        {
        if( !responses_.contentTypeDefined( op, path, statusCode, contentType))
          {
          throw new ResponseValidationException( op, path, statusCode, "body", String.format( "unexpected response contentType=%s", contentType));
          }
        
        // Compare actual body content...
        JsonNode bodyContentJson =
          bodyContentJson( op, path, statusCode, contentType, bodyContent)
          .orElseThrow( () -> new ResponseUnvalidatedException( op, path, statusCode, "body", String.format( "contentType=%s can't be validated", contentType)));

        // ...with expected content schema...
        JsonNode schema =
          responses_.contentSchema( op, path, statusCode, contentType)
          .orElseThrow( () -> new ResponseUnvalidatedException( op, path, statusCode, "body", String.format( "no schema defined for contentType=%s", contentType)));

        // ...and report any non-conformance errors
        ValidationData<Void> validation = new ValidationData<>();
        try
          {
          SchemaValidator schemaValidator = new SchemaValidator( null, schema);
          schemaValidator.validate( bodyContentJson, validation);
          }
        catch( Exception e)
          {
          throw new ResponseValidationException( op, path, statusCode, "body", "can't validate content", e);
          }
        
        if( !validation.isValid())
          {
          throw new ResponseValidationException( op, path, statusCode, "body", String.format( "invalid response\n%s", validationErrors( validation)));
          }
        }
      }
    catch( ResponseUnvalidatedException unvalidated)
      {
      notify( unvalidated);
      }
    }

  /**
   * Returns a JSON representation of the given body content. Returns {@link Optional#empty} if no JSON representation is possible.
   */
  private Optional<JsonNode> bodyContentJson( String op, String path, int statusCode, String contentType, String bodyContent)
    {
    try
      {
      MediaRange media = MediaRange.of( contentType);

      return
        Optional.ofNullable(
          ("application/json".equals( media.base()) || "json".equals( media.suffix()))?
          Optional.of( decodeJson( bodyContent))
          .filter( json -> !json.isMissingNode())
          .orElseThrow( () -> new IllegalArgumentException( "Response body is empty")):

          "text/plain".equals( media.base())?
          decodeSimple( bodyContent) :

          "multipart/form-data".equals( media.base())?
          decodeForm( bodyContent) :

          null);
      }
    catch( Exception e)
      {
      throw new ResponseValidationException( op, path, statusCode, "body", String.format( "Can't decode as contentType=%s", contentType), e);
      }
    }

  /**
   * Returns the JSON representation of JSON-encoded content.
   */
  private JsonNode decodeJson( String content) throws Exception
    {
    return new ObjectMapper().readTree( new StringReader( content));
    }

  /**
   * Returns the JSON representation of simple-encoded text content.
   */
  private JsonNode decodeSimple( String content) throws Exception
    {
    return null;
    }

  /**
   * Returns the JSON representation of multipart/form-data content.
   */
  private JsonNode decodeForm( String content) throws Exception
    {
    return null;
    }

  /**
   * Notifies a {@link ResponseUnvalidatedException} exception.
   */
  private void notify( ResponseUnvalidatedException unvalidated)
    {
    unvalidatedHandler_.accept( unvalidated);
    }

  /**
   * Returns a message describing the errors listed in the given schema validation results.
   */
  private String validationErrors( ValidationData<Void> validation)
    {
    StringJoiner joiner = new StringJoiner( "\n");
    for( ValidationItem item : validation.results().items( ValidationSeverity.ERROR))
      {
      // Extract JSON pointer fragment for the location of the failed schema assertion
      StringBuffer schemaKeys = new StringBuffer();
      String crumbs = item.schemaCrumbs();
      int keyEnd = crumbs.lastIndexOf( '>');
      boolean moreSchemaKeys = keyEnd >= 0;

      while( moreSchemaKeys)
        {
        int keyStart = crumbs.lastIndexOf( '<', keyEnd);
        moreSchemaKeys = keyStart >= 0;
        if( moreSchemaKeys)
          {
          schemaKeys.insert( 0, crumbs.substring( keyStart + 1, keyEnd));
          keyEnd = keyStart - 2;
          }

        moreSchemaKeys =
          moreSchemaKeys
          && keyEnd > 0
          && crumbs.substring( keyEnd, keyStart).equals( ">.");
        if( moreSchemaKeys)
          {
          schemaKeys.insert( 0, "/");
          }
        }

      String schemaLocation = Optional.of( schemaKeys.toString()).filter( String::isEmpty).orElse( String.format( "#%s", schemaKeys));
      String dataLocation = item.dataCrumbs();
      String location = String.format("%s%s", dataLocation, schemaLocation);
      
      joiner.add(
        String.format(
          "%s%s",
          Optional.of( location).filter( String::isEmpty).orElse( String.format( "%s: ", location)),
          item.message()));
      }
    
    return joiner.toString();
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
   * Handles a {@link ResponseUnvalidatedException} by ignoring it.
   */
  public static final Consumer<ResponseUnvalidatedException> UNVALIDATED_IGNORE = e -> {};

  /**
   * Handles a {@link ResponseUnvalidatedException} by throwing the exception.
   */
  public static final Consumer<ResponseUnvalidatedException> UNVALIDATED_FAIL = e -> { throw e;};

  private final ResponsesDef responses_;
  private Consumer<ResponseUnvalidatedException> unvalidatedHandler_ = UNVALIDATED_IGNORE;
  }
