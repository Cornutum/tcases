//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.CollectionUtils.mapping;
import static org.cornutum.tcases.openapi.test.JsonUtils.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

/**
 * Returns the JSON representation of simple-encoded content.
 */
public class SimpleDecoder extends AbstractDecoder
  {
  /**
   * Creates a new SimpleDecoder instance.
   */
  public SimpleDecoder( EncodingDef encoding)
    {
    super( encoding);

    delimiter_ =
      "pipeDelimited".equals( getStyle())?
      "\\|" :

      "spaceDelimited".equals( getStyle())?
      " " :

      ",";
    }

  /**
   * Returns the possible JSON object representations of the given content.
   */
  @Override
  public List<JsonNode> decodeObject( String content)
    {
    return
      // Is this a (possibly empty) list of array elements?...
      Optional.ofNullable( content)
      .map( text -> text.isEmpty()? new String[0] : text.split( delimiter_, -1))

      // ...either exploded key=value pairs or a sequence of key,value elements?
      .filter( members -> isExploded() || members.length % 2 == 0)
      
      .map( members -> {
        // Yes, return JSON representations for these object properties.
        List<Map.Entry<String,String>> properties =
          // Non-exploded property mappings?
          !isExploded()?
          IntStream.range( 0, members.length / 2)
          .mapToObj( i -> mapping( members[ 2*i], members[ 2*i + 1]))
          .collect( toList()) :

          // Exploded property mappings?
          Optional.of(
            Arrays.stream( members)
            .map( member -> member.split( "=", -1))
            .collect( toList()))
          .filter( pairs -> pairs.stream().allMatch( pair -> pair.length == 2))
          .map( mappings -> mappings.stream().map( mapping -> mapping( mapping[0], mapping[1])).collect( toList()))

          // Otherwise, no object recognized
          .orElse( null);
          
        return
          Optional.ofNullable( properties)
          .map( mappings -> decodeObject( mappings))
          .orElse( emptyList());
        })

      // No, not recognizable as an object
      .orElse( emptyList());
    }

  /**
   * Returns the possible JSON object representations of the given property mappings.
   */
  private List<JsonNode> decodeObject( List<Map.Entry<String,String>> properties)
    {
    return
      properties.isEmpty()?
      singletonList( createObjectNode()) :

      decodeValue( properties.get(0).getValue()).stream()
      .map( jsonNode -> newObject( properties.get(0).getKey(), jsonNode))
      .flatMap( firstProperty -> {
        return
          decodeObject( properties.subList( 1, properties.size())).stream()
          .map( JsonUtils::expectObject)
          .map( otherProperties -> appendObject( firstProperty, otherProperties));
        })
      .collect( toList());
    }

  /**
   * Returns the possible JSON array representations of the given content.
   */
  @Override
  public List<JsonNode> decodeArray( String content)
    {
    return
      Optional.ofNullable( content)
      .map( text -> text.isEmpty()? new String[0] : text.split( delimiter_, -1))
      .map( members -> decodeArray( members))
      .orElse( emptyList());
    }

  /**
   * Returns the possible JSON array representations of the given sequence of strings.
   */
  private List<JsonNode> decodeArray( String[] members)
    {
    return
      members.length == 0?
      singletonList( createArrayNode()) :

      // To preserve input order, recursively traverse members depth-first.
      decodeArray( Arrays.copyOfRange( members, 0, members.length - 1)).stream()
      .map( jsonNode -> (ArrayNode) jsonNode)
      .flatMap( prevArray -> {
        return
          decodeValue( members[ members.length - 1]).stream()
          .map( jsonNode -> createArrayNode().addAll( prevArray).add( jsonNode));
        })
      .collect( toList());
    }

  private final String delimiter_;
  }
