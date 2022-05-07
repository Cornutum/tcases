//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.CollectionUtils.toOrderedMap;
import static org.cornutum.tcases.openapi.test.JsonUtils.createObjectNode;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Returns the JSON representation of application/x-www-form-urlencoded content.
 */
public class FormUrlDecoder extends AbstractDecoder
  {
  /**
   * Creates a new FormUrlDecoder instance.
   */
  public FormUrlDecoder( ContentDef contentDef)
    {
    super( contentDef);
    valueDecoder_ = new SimpleDecoder( contentDef);
    }

  /**
   * Returns the possible JSON array representations of the given content.
   */
  @Override
  public List<JsonNode> decodeArray( String content)
    {
    return emptyList();
    }

  /**
   * Returns the JSON number representation of the given content.
   */
  @Override
  public Optional<JsonNode> decodeNumber( String content)
    {
    return Optional.empty();
    }

  /**
   * Returns the JSON boolean representation of the given content.
   */
  @Override
  public Optional<JsonNode> decodeBoolean( String content)
    {
    return Optional.empty();
    }

  /**
   * Returns the JSON string representation of the given content.
   */
  @Override
  public Optional<JsonNode> decodeString( String content)
    {
    return Optional.empty();
    }

  /**
   * Returns the possible JSON object representations of the given content.
   */
  @Override
  public List<JsonNode> decodeObject( String content)
    {
    return
      // Is this a (possibly empty) list of property mappings?...
      Optional.ofNullable( content)
      .map( text -> text.isEmpty()? new String[0] : text.split( "&", -1))
      .map( pairs -> {

        List<String[]> mappings =
          Arrays.stream( pairs)
          .map( pair -> pair.split( "=", -1))
          .collect( toList());

        mappings.stream()
          .filter( mapping -> mapping.length != 2)
          .findFirst()
          .ifPresent( mapping -> {
            throw new IllegalArgumentException(
              String.format(
                "'%s' is not a valid key/value pair",
                Arrays.stream( mapping).collect( joining( "="))));
            });
        
        Map<String,String> properties =
          mappings.stream()
          .collect(
            toOrderedMap(
              mapping -> decodeUrl( mapping[0]),
              mapping -> decodeUrl( mapping[1])));

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
      singletonList( createObjectNode()) :

      // To preserve input order, recursively traverse entries depth-first.
      decodeObject( properties.subList( 0, properties.size() - 1)).stream()
      .map( jsonNode -> (ObjectNode) jsonNode)
      .flatMap( prevObject -> {
        return
          valueDecoder_.decode( properties.get( properties.size() - 1).getValue()).stream()
          .map( jsonNode -> {
            ObjectNode nextObject = createObjectNode();
            nextObject = nextObject.setAll( prevObject);
            nextObject = nextObject.set( properties.get( properties.size() - 1).getKey(), jsonNode);
            return nextObject;
            });
        })
      .collect( toList());
    }

  private final SimpleDecoder valueDecoder_;
  }
