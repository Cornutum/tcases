//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.CollectionUtils.*;
import static org.cornutum.tcases.openapi.test.JsonUtils.*;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.toList;

/**
 * Defines the responses for requests described by an OpenAPI definition.
 */
public class ResponsesDef
  {
  /**
   * Creates a new ResponsesDef instance.
   */
  public ResponsesDef( ObjectNode root)
    {
    root_ = root;
    }

  /**
   * Returns a view of this ResponseDef that includes only the given API request paths.
   * If <CODE>paths</CODE> is null or empty, include all paths.
   */
  public ResponsesDef forPaths( Collection<String> paths)
    {
    ObjectNode view = createObjectNode();
    Optional<Collection<String>> includedPaths = Optional.ofNullable( paths).filter( p -> !p.isEmpty());

    toStream( root_.fieldNames())
      .filter( path -> includedPaths.map( includes -> includes.stream().anyMatch( include -> path.equalsIgnoreCase( include))).orElse( true))
      .forEach( path -> view.set( path, root_.get( path)));
    
    return new ResponsesDef( view);
    }

  /**
   * Returns a view of this ResponseDef that includes only the given API request paths.
   * If <CODE>paths</CODE> is null or empty, include all paths.
   */
  public ResponsesDef forPaths( String... paths)
    {
    return forPaths( Arrays.asList( paths));
    }

  /**
   * Returns a view of this ResponseDef that includes only the given API request operations.
   * If <CODE>ops</CODE> is null or empty, include all ops.
   */
  public ResponsesDef forOps( Collection<String> ops)
    {
    ObjectNode view = createObjectNode();
    Optional<Collection<String>> includedOps = Optional.ofNullable( ops).filter( o -> !o.isEmpty());

    toStream( root_.fields())
      .collect(
        toOrderedMap(
          pathDef -> pathDef.getKey(),
          
          pathDef -> {
            ObjectNode opsDef = (ObjectNode) pathDef.getValue();
            ObjectNode opsView = createObjectNode();
            toStream( opsDef.fieldNames())
              .filter( op -> includedOps.map( includes -> includes.stream().anyMatch( include -> op.equalsIgnoreCase( include))).orElse( true))
              .forEach( op -> opsView.set( op, opsDef.get( op)));

            return opsView;
          }))
      .forEach( (path, opsView) -> view.set( path, opsView));

    return new ResponsesDef( view);
    }

  /**
   * Returns a view of this ResponseDef that includes only the given API request operations.
   */
  public ResponsesDef forOps( String... ops)
    {
    return forOps( Arrays.asList( ops));
    }

  /**
   * Returns the request paths for which responses are defined.
   */
  public List<String> paths()
    {
    return toStream( root_.fieldNames()).collect( toList());
    }

  /**
   * Returns the operations on the given request path for which responses are defined.
   */
  public List<String> ops( String path)
    {
    return toStream( pathDef( path).fieldNames()).collect( toList());
    }

  /**
   * Returns the response definitions for the given path.
   */
  public ObjectNode pathDef( String path)
    {
    return (ObjectNode) root_.get( path);
    }

  /**
   * Returns the response definitions for the given path operation.
   */
  public ObjectNode opDef( String op, String path)
    {
    return (ObjectNode) pathDef( path).get( op);
    }

  /**
   * Returns true if expectations are defined when the given status code is received for the given operation on the API
   * resource at the given path.
   */
  public boolean defined( String op, String path, int statusCode)
    {
    return opStatusResponse( op, path, statusCode).isPresent();
    }

  /**
   * Returns true if a response body is expected when the given status code is received for the given operation on the API
   * resource at the given path.
   */
  public boolean hasBody( String op, String path, int statusCode)
    {
    return opStatusContent( op, path, statusCode).isPresent();
    }

  /**
   * Returns the specified response body content definition for the given status code for the given operation on the API resource at the given path.
   */
  public Optional<ContentDef> bodyContentDef( String op, String path, int statusCode, String contentType)
    {
    return
      opStatusContent( op, path, statusCode, contentType)
      .map( content -> new ContentDef( contentType, expectObject( content.get( "schema")), null, contentEncodings( content)));
    }

  /**
   * Returns the encodings for properties of the given response content.
   */
  private Map<String,EncodingDef> contentEncodings( JsonNode content)
    {
    return
      Optional.ofNullable( expectObject( content.get( "encoding")))
      .map( encodings -> toStream( encodings.fields()))
      .orElse( Stream.empty())
      .collect( toOrderedMap( encoding -> encoding.getKey(), encoding -> contentEncodingDef( expectObject( encoding.getValue()))));
    }

  /**
   * Returns the encoding for the value of a property of a response object for the given status code and content type for the
   * given operation on the API resource at the given path.
   */
  private EncodingDef contentEncodingDef( ObjectNode encoding)
    {
    String style = Optional.ofNullable( encoding.get( "style")).map( JsonNode::asText).orElse( null);
    Boolean exploded = Optional.ofNullable( encoding.get( "explode")).map( JsonNode::asBoolean).orElse( null);
    String contentType = Optional.ofNullable( encoding.get( "contentType")).map( JsonNode::asText).orElse( null);

    return
      Optional.ofNullable( style)
      .map( partStyle -> EncodingDef.forUrlEncodedForm( style, exploded))
      .orElse( EncodingDef.forMultipartForm( contentType, null));
    }

  /**
   * Returns the names of headers defined for the given status code for the given operation on the API resource at the given path.
   */
  public List<HeaderDef> headerDefs( String op, String path, int statusCode)
    {
    return
      opStatusResponse( op, path, statusCode)
      .map( this::contentHeaders)
      .orElse( emptyList());
    }

  /**
   * Returns the response definitions for the given operation on the API resource at the given path.
   */
  private ObjectNode opResponses( String op, String path)
    {
    return
      objectAt( path, op.toLowerCase())
      .orElseThrow( () -> new IllegalArgumentException( String.format( "%s %s: no OpenAPI response definitions found", op, path)));
    }

  /**
   * Returns the response definition for the given status code for the given operation on the API resource at the given path.
   */
  private Optional<ObjectNode> opStatusContent( String op, String path, int statusCode)
    {
    return
      opStatusResponse( op, path, statusCode)
      .flatMap( response -> Optional.ofNullable( expectObject( response.get( "content"))))
      .filter( content -> content.size() > 0);
    }

  /**
   * Returns the response definition for the given status code and content type for the given operation on the API resource at the given path.
   */
  private Optional<ObjectNode> opStatusContent( String op, String path, int statusCode, String contentType)
    {
    return
      opStatusContent( op, path, statusCode)
      .flatMap( content -> {
        MediaRange media = MediaRange.of( contentType);

        Object[] alternatives = new Object[]{
          contentType,
          media.baseStructured(),
          MediaRange.anyOf( media.type(), media.suffix()),
          media.base(),
          MediaRange.anyOf( media.type()),
          MediaRange.any()};

        return
          Arrays.stream( alternatives)
          .map( type -> expectObject( content.get( String.valueOf( type))))
          .filter( Objects::nonNull)
          .findFirst();
        });
    }

  /**
   * Returns the header definitions for the given content.
   */
  private List<HeaderDef> contentHeaders( ObjectNode content)
    {
    return
      Optional.ofNullable( expectObject( content.get( "headers")))
      .map( headers -> toStream( headers.fields()))
      .orElse( Stream.empty())
      .map( headerDef -> {
        String name = headerDef.getKey();
        ObjectNode def = expectObject( headerDef.getValue());

        Boolean required =
          Optional.ofNullable( def.get( "required"))
          .map( JsonNode::asBoolean)
          .orElse( null);

        Boolean exploded =
          Optional.ofNullable( def.get( "explode"))
          .map( JsonNode::asBoolean)
          .orElse( null);
        
        Optional<ObjectNode> contentDef = Optional.ofNullable( expectObject( def.get( "content")));
        String contentType =
          contentDef
          .map( ObjectNode::fieldNames)
          .flatMap( contentTypes -> toStream( contentTypes).findFirst()) 
          .orElse( "text/plain");

        Optional<ObjectNode> headerContent =
          contentDef
          .map( c -> expectObject( c.get( contentType)));

        ObjectNode schema =
          headerContent
          .map( c -> expectObject( c.get( "schema")))
          .orElse( expectObject( def.get( "schema")));

        Map<String,EncodingDef> propertyEncodings =
          headerContent
          .map( this::contentEncodings)
          .orElse( emptyMap());

        EncodingDef valueEncoding = EncodingDef.forSimpleValue( exploded);
        
        return new HeaderDef( name, required, new ContentDef( contentType, schema, valueEncoding, propertyEncodings));
        })
      .collect( toList());      
    }

  /**
   * Returns the response definition for the given status code for the given operation on the API resource at the given path.
   */
  private Optional<ObjectNode> opStatusResponse( String op, String path, int statusCode)
    {
    ObjectNode opResponses = opResponses( op, path);
    String statusKey = String.valueOf( statusCode);
    String statusRangeKey = String.format( "%sXX", statusKey.substring( 0, 1));

    return
      Arrays.asList( statusKey, statusRangeKey, "default").stream()
      .map( key -> expectObject( opResponses.get( key)))
      .filter( Objects::nonNull)
      .findFirst();
    }

  /**
   * Returns the JSON object node at the given path.
   */
  private Optional<ObjectNode> objectAt( String... path)
    {
    return Optional.ofNullable( expectObject( root_.at( pointer( path))));
    }

  /**
   * Writes a JSON representation of response definitions to the given output stream.
   */
  public static void write( ResponsesDef responses, Writer writer)
    {
    try( JsonGenerator generator = mapper().writerWithDefaultPrettyPrinter().createGenerator( writer))
      {
      mapper().writeTree( generator, responses.root_);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't write JSON document", e);
      }
    }

  /**
   * Reads a JSON representation of response definitions from the given input stream.
   */
  public static ResponsesDef read( Reader reader)
    {
    try
      {
      return
        new ResponsesDef(
          Optional.of( mapper().readTree( reader))
          .filter( JsonNode::isObject)
          .map( root -> (ObjectNode) root)
          .orElseThrow( () -> new IllegalStateException( "Expected JSON type=object")));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read JSON document", e);
      }
    }

  @Override
  public String toString()
    {
    return ToString.builder( getClass()).toString();
    }

  @Override
  public boolean equals( Object object)
    {
    ResponsesDef other =
      object != null && object.getClass().equals( getClass())?
      (ResponsesDef) object :
      null;

    return
      other != null
      && Objects.equals( other.root_, root_);
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( root_);
    }
  
  private final ObjectNode root_;
  }
