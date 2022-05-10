//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import static java.util.stream.Collectors.toList;

/**
 * Defines methods for processing JSON values.
 */
public final class JsonUtils
  {
  /**
   * Creates a new JsonUtils instance.
   */
  private JsonUtils()
    {
    // Static methods only.
    }

  /**
   * Returns the given JSON node as an object node.
   */
  public static ObjectNode expectObject( JsonNode node)
    {
    return
      isMissing( node)
      ? null
      : asObject( node).orElseThrow( () -> new IllegalStateException( String.format( "Expected type=OBJECT, found type=%s", node.getNodeType())));
    }

  /**
   * Returns the given JSON node if it is an object node. Otherwise returns {@link Optional#empty}.
   */
  public static Optional<ObjectNode> asObject( JsonNode node)
    {
    return
      isMissing( node)
      ? Optional.empty()
      : Optional.of( node).filter( JsonNode::isObject).map( object -> (ObjectNode) object);
    }

  /**
   * Returns a new ObjectNode containing given field value.
   */
  public static ObjectNode newObject( String field, JsonNode value)
    {
    return createObjectNode().set( field, value);
    }

  /**
   * Returns a new ObjectNode that is the same as the given object but adds all fields of the other object.
   */
  public static ObjectNode appendObject( ObjectNode object, ObjectNode otherObject)
    {
    ObjectNode appended = createObjectNode();
    appended.setAll( object);
    appended.setAll( otherObject);
    return appended;
    }

  /**
   * Returns the given JSON node as an array node.
   */
  public static ArrayNode expectArray( JsonNode node)
    {
    return
      isMissing( node)
      ? null
      : asArray( node).orElseThrow( () -> new IllegalStateException( String.format( "Expected type=ARRAY, found type=%s", node.getNodeType())));
    }

  /**
   * Returns the given JSON node if it is an array node. Otherwise returns {@link Optional#empty}.
   */
  public static Optional<ArrayNode> asArray( JsonNode node)
    {
    return
      isMissing( node)
      ? Optional.empty()
      : Optional.of( node).filter( JsonNode::isArray).map( array -> (ArrayNode) array);
    }

  /**
   * Returns a new ArrayNode containing given element
   */
  public static ArrayNode newArray( JsonNode element)
    {
    return createArrayNode().add( element);
    }

  /**
   * Returns a new ArrayNode that is the same as the given array but adds all elements of the other array.
   */
  public static ArrayNode appendArray( ArrayNode array, ArrayNode otherArray)
    {
    return createArrayNode().addAll( array).addAll( otherArray);
    }

  /**
   * Returns true if the given JSON node is null or missing.
   */
  public static boolean isMissing( JsonNode node)
    {
    return
      Optional.ofNullable( node)
      .map( JsonNode::isMissingNode)
      .orElse( true);
    }

  /**
   * Returns true if the given string represents an integer
   */
  public static boolean isInteger( String value)
    {
    try
      {
      Integer.parseInt( value);
      return true;
      }
    catch( Exception e)
      {
      return false;
      }
    }

  /**
   * Returns a JSON pointer for the given path
   */
  public static JsonPointer pointer( String... path)
    {
    return pointer( Arrays.asList( path));
    }

  /**
   * Returns a JSON pointer for the given path
   */
  public static JsonPointer pointer( Iterable<String> path)
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
  private static String pointerSegment( String name)
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
   * Returns the final segment of the given JSON pointer.
   */
  public static String tailOf( JsonPointer pointer)
    {
    return
      Optional.of( pathOf( pointer))
      .filter( path -> !path.isEmpty())
      .map( path -> path.get( path.size() - 1))
      .orElse( "");
    }

  /**
   * Returns the path represented by the given JSON pointer.
   */
  public static List<String> pathOf( JsonPointer pointer)
    {
    String[] segments = String.valueOf( pointer).split( "/", 0);
    return
      IntStream.range( 1, segments.length)
      .mapToObj( i -> pathElementOf( segments[i]))
      .collect( toList());
    }

  /**
   * Returns the path element represented by the given JSON pointer segment.
   */
  private static String pathElementOf( String segment)
    {
    Matcher matcher = POINTER_ESCAPED.matcher( segment);
    StringBuffer unescaped = new StringBuffer();
    while( matcher.find())
      {
      matcher.appendReplacement(
        unescaped,
        matcher.group(1) != null? "~" : "/");
      }
    matcher.appendTail( unescaped);
    return unescaped.toString();
    }

  /**
   * Returns a new empty ObjectNode.
   */
  public static ObjectNode createObjectNode()
    {
    return mapper().createObjectNode();
    }

  /**
   * Returns a new empty ArrayNode.
   */
  public static ArrayNode createArrayNode()
    {
    return mapper().createArrayNode();
    }

  /**
   * Returns a JSON ObjectMapper.
   */
  public static ObjectMapper mapper()
    {
    return mapper_;
    }

  /**
   * Returns the JSON value represented by the given content.
   */
  public static JsonNode readJson( String content) throws IOException
    {
    return readJson( new StringReader( content));
    }

  /**
   * Returns the JSON value represented by content from the given Reader.
   */
  public static JsonNode readJson( Reader content) throws IOException
    {
    return mapper().readTree( content);
    }

  private static final Pattern POINTER_ESCAPES = Pattern.compile( "(~)|(/)");
  private static final Pattern POINTER_ESCAPED = Pattern.compile( "(~0)|(~1)");
  private static final ObjectMapper mapper_ = new ObjectMapper();
  }
