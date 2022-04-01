//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.IntStream;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Returns the JSON representation of simple-encoded content.
 */
public class SimpleDecoder extends AbstractDecoder
  {
  /**
   * Creates a new SimpleDecoder instance.
   */
  public SimpleDecoder( boolean explode)
    {
    super( explode);
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
      .map( text -> text.isEmpty()? new String[0] : text.split( ",", -1))

      // ...either exploded key=value pairs or a sequence of key,value elements?
      .filter( members -> isExplode() || members.length % 2 == 0)
      
      .map( members -> {

        Map<String,String> properties =
          // Non-exploded property mappings?
          !isExplode()?
          IntStream.range( 0, members.length / 2)
          .mapToObj( Integer::valueOf)
          .collect( toOrderedMap( i -> members[ 2*i], i -> members[ 2*i + 1])) :

          // Exploded property mappings?
          Optional.of(
            Arrays.stream( members)
            .map( member -> member.split( "=", -1))
            .collect( toList()))
          .filter( pairs -> pairs.stream().allMatch( pair -> pair.length == 2))
          .map( mappings -> mappings.stream().collect( toOrderedMap( mapping -> mapping[0], mapping -> mapping[1])))

          // Otherwise, no object recognized
          .orElse( null);

        // Return JSON representations for these object properties.
        return
          Optional.ofNullable( properties)
          .map( map -> decodeObject( map.entrySet().stream().collect( toList())))
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
      singletonList( mapper_.createObjectNode()) :

      // To preserve input order, recursively traverse entries depth-first.
      decodeObject( properties.subList( 0, properties.size() - 1)).stream()
      .map( jsonNode -> (ObjectNode) jsonNode)
      .flatMap( prevObject -> {
        return
          decodeValue( properties.get( properties.size() - 1).getValue()).stream()
          .map( jsonNode -> {
            ObjectNode nextObject = mapper_.createObjectNode();
            nextObject = nextObject.setAll( prevObject);
            nextObject = nextObject.set( properties.get( properties.size() - 1).getKey(), jsonNode);
            return nextObject;
            });
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
      .map( text -> text.isEmpty()? new String[0] : text.split( ",", -1))
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
      singletonList( mapper_.createArrayNode()) :

      // To preserve input order, recursively traverse members depth-first.
      decodeArray( Arrays.copyOfRange( members, 0, members.length - 1)).stream()
      .map( jsonNode -> (ArrayNode) jsonNode)
      .flatMap( prevArray -> {
        return
          decodeValue( members[ members.length - 1]).stream()
          .map( jsonNode -> mapper_.createArrayNode().addAll( prevArray).add( jsonNode));
        })
      .collect( toList());
    }

  /**
   * A collector that produces a map sorted in insertion order.
   */
  private <T> Collector<T,?,Map<String,String>> toOrderedMap( Function<T,String> keyMapper, Function<T,String> valueMapper)
    {
    return
      toMap(
        keyMapper,
        valueMapper,
        (v1, v2) -> v1,
        LinkedHashMap::new);
    }

  private final ObjectMapper mapper_ = new ObjectMapper();        
  }
