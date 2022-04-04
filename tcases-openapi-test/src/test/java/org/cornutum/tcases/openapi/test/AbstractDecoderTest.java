//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;

import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.net.URLEncoder;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.joining;

/**
 * Base class for decoder tests.
 */
public abstract class AbstractDecoderTest
  {
  protected void assertJsonNodes( String label, List<JsonNode> actual, String... expected)
    {
    assertThat(
      label,
      actual.stream().map( String::valueOf).toArray( String[]::new),
      listsElements( expected));
    }

  protected List<JsonNode> objectNodes( List<JsonNode> jsonNodes)
    {
    jsonNodes.stream()
      .forEach( node -> {
        assertThat( String.format( "'%s' is an object", node), node.isObject(), is( true));
        });

    return jsonNodes;
    }

  protected List<JsonNode> arrayNodes( List<JsonNode> jsonNodes)
    {
    jsonNodes.stream()
      .forEach( node -> {
        assertThat( String.format( "'%s' is an array", node), node.isArray(), is( true));
        });

    return jsonNodes;
    }

  protected List<JsonNode> valueNodes( List<JsonNode> jsonNodes)
    {
    jsonNodes.stream()
      .forEach( node -> {
        assertThat( String.format( "'%s' is a value", node), node.isValueNode(), is( true));
        });

    return jsonNodes;
    }

  /**
   * Builds an application/x-www-form-urlencoded string.
   */
  protected static class FormUrlBuilder
    {
    /**
     * Creates a new FormUrlBuilder instance.
     */
    private FormUrlBuilder()
      {
      }

    /**
     * Returns a new FormUrlBuilder.
     */
    public static FormUrlBuilder with()
      {
      return new FormUrlBuilder();
      }

    /**
     * Adds a field value to this form.
     */
    public FormUrlBuilder field( String name, String value)
      {
      fields_.put( encodeUrl( name), encodeUrl( value));
      return this;
      }

    /**
     * Returns the encoded form string.
     */
    public String build()
      {
      return
        fields_.entrySet().stream()
        .map( field -> String.format( "%s=%s", field.getKey(), field.getValue()))
        .collect( joining( "&"));
      }

    /**
     * Enccodes an application/x-www-form-urlencoded string.
     */
    protected String encodeUrl( String content)
      {
      try
        {
        return
          content == null
          ? null
          : URLEncoder.encode( content, "UTF-8");
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( String.format( "Can't encode application/x-www-form-urlencoded content=%s", content), e);
        }
      }

    private Map<String,String> fields_ = new LinkedHashMap<String,String>();
    }

  }
