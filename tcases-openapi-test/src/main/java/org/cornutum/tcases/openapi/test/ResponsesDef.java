//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Objects;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

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
   * Returns true if the given content type is defined for the given status code for the given operation on the API resource at the given path.
   */
  public boolean contentTypeDefined( String op, String path, int statusCode, String contentType)
    {
    return opStatusContent( op, path, statusCode, contentType).isPresent();
    }

  /**
   * Returns the response schema for the given status code and content type for the given operation on the API resource at the given path.
   */
  public Optional<ObjectNode> contentSchema( String op, String path, int statusCode, String contentType)
    {
    return
      opStatusContent( op, path, statusCode, contentType)
      .flatMap( content -> Optional.ofNullable( asObject( content.get( "schema"))));
    }

  /**
   * Returns the names of headers defined for the given status code for the given operation on the API resource at the given path.
   */
  public String[] headers( String op, String path, int statusCode)
    {
    return
      opStatusHeaders( op, path, statusCode)
      .map( headers -> toStream( headers.fieldNames()).toArray( String[]::new))
      .orElse( new String[0]);
    }

  /**
   * Returns true if the given header is required for the given status code for the given operation on the API resource at the given path.
   */
  public boolean headerRequired( String op, String path, int statusCode, String headerName)
    {
    return
      opStatusHeader( op, path, statusCode, headerName)
      .flatMap( header -> Optional.ofNullable( header.get( "required")))
      .map( JsonNode::asBoolean)
      .orElse( false);
    }

  /**
   * Returns true if explode-encoding is used for the given header value for the given status code for the given operation on the API resource at the given path.
   */
  public boolean headerExplode( String op, String path, int statusCode, String headerName)
    {
    return
      opStatusHeader( op, path, statusCode, headerName)
      .flatMap( header -> Optional.ofNullable( header.get( "explode")))
      .map( JsonNode::asBoolean)
      .orElse( false);
    }

  /**
   * Returns the content type for the given header value for the given status code for the given operation on the API resource at the given path.
   */
  public Optional<String> headerContentType( String op, String path, int statusCode, String headerName)
    {
    return
      opStatusHeaderContent( op, path, statusCode, headerName)
      .flatMap( content -> toStream( content.fieldNames()).findFirst());
    }

  /**
   * Returns the schema used for the given header value for the given status code for the given operation on the API resource at the given path.
   */
  public Optional<ObjectNode> headerSchema( String op, String path, int statusCode, String headerName)
    {
    Optional<ObjectNode> content =
      opStatusHeaderContent( op, path, statusCode, headerName)
      .flatMap( mediaTypes -> toStream( mediaTypes.elements()).findFirst())
      .map( contentType -> asObject( contentType));

    return
      (content.isPresent()? content : opStatusHeader( op, path, statusCode, headerName))
      .flatMap( header -> Optional.ofNullable( asObject( header.get( "schema"))));
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
      .flatMap( response -> Optional.ofNullable( asObject( response.get( "content"))))
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
          .map( type -> asObject( content.get( String.valueOf( type))))
          .filter( Objects::nonNull)
          .findFirst();
        })
      ;
    }

  /**
   * Returns the header definitions for the given status code for the given operation on the API resource at the given path.
   */
  private Optional<ObjectNode> opStatusHeaders( String op, String path, int statusCode)
    {
    return
      opStatusResponse( op, path, statusCode)
      .flatMap( response -> Optional.ofNullable( asObject( response.get( "headers"))))
      .filter( headers -> headers.size() > 0);
    }

  /**
   * Returns the definition of the given header defined for the given status code for the given operation on the API resource at the given path.
   */
  private Optional<ObjectNode> opStatusHeader( String op, String path, int statusCode, String headerName)
    {
    return
      opStatusHeaders( op, path, statusCode)
      .flatMap( headers -> Optional.ofNullable( asObject( headers.get( headerName))));
    }

  /**
   * Returns the definition of the given header content defined for the given status code for the given operation on the API resource at the given path.
   */
  private Optional<ObjectNode> opStatusHeaderContent( String op, String path, int statusCode, String headerName)
    {
    return
      opStatusHeader( op, path, statusCode, headerName)
      .flatMap( header -> Optional.ofNullable( asObject( header.get( "content"))));
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
      .map( key -> asObject( opResponses.get( key)))
      .filter( Objects::nonNull)
      .findFirst();
    }

  /**
   * Returns the JSON object node at the given path.
   */
  private Optional<ObjectNode> objectAt( String... path)
    {
    return Optional.ofNullable( asObject( root_.at( pointer( path))));
    }

  /**
   * Returns the given JSON node as an object node.
   */
  private ObjectNode asObject( JsonNode node)
    {
    return
      Optional.ofNullable( node)
      .filter( notNull -> !notNull.isMissingNode())

      .map(
        notMissing ->
        Optional.of( notMissing)
        .filter( JsonNode::isObject)
        .map( object -> (ObjectNode) object)
        .orElseThrow( () -> new IllegalStateException( String.format( "Expected type=OBJECT, found type=%s", node.getNodeType()))))

      .orElse( null);
    }

  /**
   * Returns a JSON pointer for the given path
   */
  private JsonPointer pointer( String... path)
    {
    StringJoiner joiner = new StringJoiner( "/", "/", "");
    for( String name : path)
      {
      joiner.add( pointerSegment( name));
      }

    return JsonPointer.compile( joiner.toString());
    }

  /**
   * Returns the given name as JSON Pointer segment, escaping special characters as defined in <A href="https://datatracker.ietf.org/doc/html/rfc6901">RFC6901</A>.
   */
  private String pointerSegment( String name)
    {
    Matcher matcher = POINTER_ESCAPES.matcher( name);
    StringBuffer escaped = new StringBuffer();
    while( matcher.find())
      {
      matcher.appendReplacement(
        escaped,
        matcher.group(1) != null? "~0" : "~1");
      }
    matcher.appendTail( escaped);
    return escaped.toString();
    }

  /**
   * Returns a stream that produces the sequence defined by the given Iterator.
   */
  private static <T> Stream<T> toStream( Iterator<T> iterator)
    {
    return
      Optional.ofNullable( iterator)
      .map( i -> {
        Iterable<T> iterable = () -> i;
        return toStream( iterable);
        })
      .orElse( null);
    }

  /**
   * Returns a stream that produces the sequence defined by the given Iterable.
   */
  private static <T> Stream<T> toStream( Iterable<T> iterable)
    {
    return
      Optional.ofNullable( iterable)
      .map( i -> StreamSupport.stream( i.spliterator(), false))
      .orElse( null);
    }

  /**
   * Writes a JSON representation of response definitions to the given output stream.
   */
  public static void write( ResponsesDef responses, Writer writer)
    {
    ObjectMapper mapper = new ObjectMapper();
    try( JsonGenerator generator = mapper.writerWithDefaultPrettyPrinter().createGenerator( writer))
      {
      mapper.writeTree( generator, responses.root_);
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
      ObjectMapper mapper = new ObjectMapper();
      return
        new ResponsesDef(
          Optional.of( mapper.readTree( reader))
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
    return String.valueOf( root_);
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

  private static final Pattern POINTER_ESCAPES = Pattern.compile( "(~)|(/)");
  }
