//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.DecimalNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import static java.util.stream.Collectors.toList;

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
  protected List<JsonNode> decodeValue( String content)
    {
    
    return
      Arrays.asList( decodeNumber( content), decodeString( content), decodeNull( content))
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
    try
      {
      return Optional.of( DecimalNode.valueOf( new BigDecimal( content)));
      }
    catch( Exception e)
      {
      return Optional.empty();
      }
    }

  /**
   * Returns the JSON string representation of the given content.
   */
  public Optional<JsonNode> decodeString( String content)
    {
    return Optional.ofNullable( TextNode.valueOf( content));
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

  private final boolean explode_;
  }
