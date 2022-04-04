//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.BigIntegerNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.DecimalNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URLDecoder;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collector;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Base class for response content decoders.
 */
public abstract class AbstractDecoder
  {
  /**
   * Creates a new AbstractDecoder instance.
   */
  protected AbstractDecoder()
    {
    this( false);
    }
  
  /**
   * Creates a new AbstractDecoder instance.
   */
  protected AbstractDecoder( boolean explode)
    {
    explode_ = explode;
    }

  /**
   * Returns true if this decoder expects objects to be encoded in exploded form.
   */
  public boolean isExplode()
    {
    return explode_;
    }

  /**
   * Returns the possible JSON representations of the given content.
   */
  public List<JsonNode> decode( String content)
    {
    return
      Arrays.asList( decodeObject( content), decodeArray( content), decodeValue( content))
      .stream()
      .flatMap( jsons -> jsons.stream())
      .collect( toList());
    }

  /**
   * Returns the possible JSON object representations of the given content.
   */
  public abstract List<JsonNode> decodeObject( String content);

  /**
   * Returns the possible JSON array representations of the given content.
   */
  public abstract List<JsonNode> decodeArray( String content);

  /**
   * Returns the possible JSON value representations of the given content.
   */
  public List<JsonNode> decodeValue( String content)
    {
    return
      Arrays.asList( decodeNumber( content), decodeBoolean( content), decodeString( content), decodeNull( content))
      .stream()
      .filter( Optional::isPresent)
      .map( Optional::get)
      .collect( toList());
    }

  /**
   * Returns the JSON number representation of the given content.
   */
  public Optional<JsonNode> decodeNumber( String content)
    {
    JsonNode jsonNode;

    try
      {
      BigDecimal number = new BigDecimal( content);
      BigInteger integer;
      try
        {
        integer =
          Optional.of( number)
          .filter( decimal -> decimal.scale() == 0)
          .map( BigDecimal::toBigIntegerExact)
          .orElse( null);
        }
      catch( Exception e)
        {
        integer = null;
        }

      jsonNode =
        Optional.ofNullable( integer)
        .map( i -> (JsonNode) BigIntegerNode.valueOf(i))
        .orElse( DecimalNode.valueOf( number));
      }
    catch( Exception e)
      {
      jsonNode = null;
      }

    return Optional.ofNullable( jsonNode);
    }

  /**
   * Returns the JSON boolean representation of the given content.
   */
  public Optional<JsonNode> decodeBoolean( String content)
    {
    return
      Optional.ofNullable( content)
      .filter( text -> "true".equals( text) || "false".equals( text))
      .map( text -> BooleanNode.valueOf( Boolean.valueOf( text)));
    }

  /**
   * Returns the JSON string representation of the given content.
   */
  public Optional<JsonNode> decodeString( String content)
    {
    return
      Optional.ofNullable( content)
      .map( text -> TextNode.valueOf( text));
    }

  /**
   * Returns the JSON null representation of the given content.
   */
  public Optional<JsonNode> decodeNull( String content)
    {
    return
      Optional.of( Objects.toString( content, ""))
      .filter( String::isEmpty)
      .map( empty -> NullNode.instance);
    }

  /**
   * A collector that produces a map sorted in insertion order.
   */
  protected static <T> Collector<T,?,Map<String,String>> toOrderedMap( Function<T,String> keyMapper, Function<T,String> valueMapper)
    {
    return
      toMap(
        keyMapper,
        valueMapper,
        (v1, v2) -> v1,
        LinkedHashMap::new);
    }

  /**
   * Decodes an application/x-www-form-urlencoded string.
   */
  protected String decodeUrl( String content)
    {
    try
      {
      return
        content == null
        ? null
        : URLDecoder.decode( content, "UTF-8");
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( String.format( "Can't decode application/x-www-form-urlencoded content=%s", content), e);
      }
    }

  private final boolean explode_;
  }
