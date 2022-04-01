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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.function.Consumer;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

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
        List<JsonNode> bodyContentJson = bodyContentJson( op, path, statusCode, contentType, bodyContent);
        if( bodyContentJson.isEmpty())
          {
          throw new ResponseUnvalidatedException( op, path, statusCode, "body", String.format( "contentType=%s can't be validated", contentType));
          }

        // ...with expected content schema...
        JsonNode schema =
          responses_.contentSchema( op, path, statusCode, contentType)
          .orElseThrow( () -> new ResponseUnvalidatedException( op, path, statusCode, "body", String.format( "no schema defined for contentType=%s", contentType)));

        // ...and report any non-conformance errors
        ValidationData<Void> validation;
        try
          {
          SchemaValidator schemaValidator = new SchemaValidator( null, schema);
          validation = validateSchema( schemaValidator, bodyContentJson);
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
   * Given a response with the given status code to the given operation on the API resource at the given path, completes
   * successfully if the response headers conform to its OpenAPI definition. Otherwise, throws a ResponseValidationException.
   *
   * @param headers Maps each header name to its value
   */
  public void assertHeadersValid( String op, String path, int statusCode, Map<String,String> headers) throws ResponseValidationException
    {
    try
      {
      if( !responses_.defined( op, path, statusCode))
        {
        throw new ResponseValidationException( op, path, String.format( "no response defined for statusCode=%s", statusCode));
        }

      // For each header defined for this response...
      Arrays.stream( responses_.headers( op, path, statusCode))
        .forEach( headerName -> {
          // ... except Content-Type, which must be ignored...
          if( !"Content-Type".equals( headerName))
            {
            if( headers.containsKey( headerName))
              {
              // ...compare expected content schema...
              JsonNode schema =
                responses_.headerSchema( op, path, statusCode, headerName)
                .orElseThrow( () -> new ResponseUnvalidatedException( op, path, statusCode, headerName, "no schema defined"));

              // ...with actual header content...
              String contentType =
                responses_.headerContentType( op, path, statusCode, headerName)
                .orElse( "text/plain");

              boolean explode = responses_.headerExplode( op, path, statusCode, headerName);
              
              List<JsonNode> headerContentJson = contentJson( op, path, statusCode, headerName, contentType, headers.get( headerName), explode);
              if( headerContentJson.isEmpty())
                {
                throw new ResponseUnvalidatedException( op, path, statusCode, headerName, String.format( "contentType=%s can't be validated", contentType));
                }

              // ...and report any non-conformance errors
              ValidationData<Void> validation;
              try
                {
                SchemaValidator schemaValidator = new SchemaValidator( null, schema);
                validation = validateSchema( schemaValidator, headerContentJson);
                }
              catch( Exception e)
                {
                throw new ResponseValidationException( op, path, statusCode, headerName, "can't validate value", e);
                }
        
              if( !validation.isValid())
                {
                throw new ResponseValidationException( op, path, statusCode, headerName, String.format( "invalid value\n%s", validationErrors( validation)));
                }
              }
            else if ( responses_.headerRequired( op, path, statusCode, headerName))
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
    }

  /**
   * Returns the results of applying the given schema validator to the given alternative content representations.
   */
  private ValidationData<Void> validateSchema( SchemaValidator schemaValidator, List<JsonNode> content) throws Exception
    {
    ValidationData<Void> initialValidation = new ValidationData<>();
    JsonNode initialContent = content.get(0);
    schemaValidator.validate( initialContent, initialValidation);

    return
      // First content alternative valid?
      initialValidation.isValid()?

      // Yes
      initialValidation :

      // No, return the results of the first successful validation of a different alternative.
      // If none validated, return the initial validation errors.
      content.subList( 1, content.size()).stream()
        .map( alternateContent -> {
          ValidationData<Void> alternateValidation = new ValidationData<>();
          schemaValidator.validate( alternateContent, alternateValidation);
          return alternateValidation;
          })
        .filter( ValidationData::isValid)
        .findFirst()
        .orElse( initialValidation);
    }

  /**
   * Returns JSON representations of the given body content. Returns {@link Optional#empty} if no JSON representation is possible.
   */
  private List<JsonNode> bodyContentJson( String op, String path, int statusCode, String contentType, String bodyContent)
    {
    return contentJson( op, path, statusCode, "body", contentType, bodyContent, false);
    }

  /**
   * Returns JSON representations of the given content. Returns {@link Collections#emptyList} if no JSON representation is possible.
   */
  private List<JsonNode> contentJson( String op, String path, int statusCode, String location, String contentType, String content, boolean explode)
    {
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
          decodeSimple( content, explode) :

          "multipart/form-data".equals( media.base())?
          decodeForm( content, explode) :

          "application/x-www-form-urlencoded".equals( media.base())?
          decodeFormUrl( content, explode) :
          
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
    return singletonList( new ObjectMapper().readTree( new StringReader( content)));
    }

  /**
   * Returns JSON representations of simple-encoded text content.
   */
  private List<JsonNode> decodeSimple( String content, boolean explode) throws Exception
    {
    return new SimpleDecoder( explode).decode( content);
    }

  /**
   * Returns JSON representation of multipart/form-data content.
   */
  private List<JsonNode> decodeForm( String content, boolean explode) throws Exception
    {
    return null;
    }

  /**
   * Returns JSON representation of application/x-www-form-urlencoded content.
   */
  private List<JsonNode> decodeFormUrl( String content, boolean explode) throws Exception
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
