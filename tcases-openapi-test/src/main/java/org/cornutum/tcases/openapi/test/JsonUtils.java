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

import java.util.Optional;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
  public static ObjectNode asObject( JsonNode node)
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
  public static JsonPointer pointer( String... path)
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

  private static final Pattern POINTER_ESCAPES = Pattern.compile( "(~)|(/)");
  private static final ObjectMapper mapper_ = new ObjectMapper();
  }
