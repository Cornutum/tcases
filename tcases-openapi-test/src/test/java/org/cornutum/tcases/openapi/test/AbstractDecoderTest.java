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

import java.util.List;

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

  }
