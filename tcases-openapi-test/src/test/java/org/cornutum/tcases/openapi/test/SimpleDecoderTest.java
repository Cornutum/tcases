//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;

/**
 * Runs {@link SimpleDecoder} tests.
 */
public class SimpleDecoderTest
  {
  @Test
  public void whenNotExplodedObject()
    {
    // Given...
    boolean explode = false;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;

    // When...
    content = "A,1,B,-2.0,C,3";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decodeObject( content),
      "{\"A\":1,\"B\":-2.0,\"C\":3}",
      "{\"A\":1,\"B\":-2.0,\"C\":\"3\"}",
      "{\"A\":1,\"B\":\"-2.0\",\"C\":3}",
      "{\"A\":1,\"B\":\"-2.0\",\"C\":\"3\"}",
      "{\"A\":\"1\",\"B\":-2.0,\"C\":3}",
      "{\"A\":\"1\",\"B\":-2.0,\"C\":\"3\"}",
      "{\"A\":\"1\",\"B\":\"-2.0\",\"C\":3}",
      "{\"A\":\"1\",\"B\":\"-2.0\",\"C\":\"3\"}");

    // When...
    content = "1,A,-2.0,,3,";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decodeObject( content)),
      "{\"1\":\"A\",\"-2.0\":\"\",\"3\":\"\"}",
      "{\"1\":\"A\",\"-2.0\":\"\",\"3\":null}",
      "{\"1\":\"A\",\"-2.0\":null,\"3\":\"\"}",
      "{\"1\":\"A\",\"-2.0\":null,\"3\":null}");

    // When...
    content = "";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decodeObject( content)),
      "{}");

    // When...
    content = "A";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));

    // When...
    content = "A,,B";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));
    }
  
  @Test
  public void whenExplodedObject()
    {
    // Given...
    boolean explode = true;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;

    // When...
    content = "A=1,B=-2.0,C=3";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decodeObject( content),
      "{\"A\":1,\"B\":-2.0,\"C\":3}",
      "{\"A\":1,\"B\":-2.0,\"C\":\"3\"}",
      "{\"A\":1,\"B\":\"-2.0\",\"C\":3}",
      "{\"A\":1,\"B\":\"-2.0\",\"C\":\"3\"}",
      "{\"A\":\"1\",\"B\":-2.0,\"C\":3}",
      "{\"A\":\"1\",\"B\":-2.0,\"C\":\"3\"}",
      "{\"A\":\"1\",\"B\":\"-2.0\",\"C\":3}",
      "{\"A\":\"1\",\"B\":\"-2.0\",\"C\":\"3\"}");

    // When...
    content = "1=A,-2.0=,3=";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decodeObject( content)),
      "{\"1\":\"A\",\"-2.0\":\"\",\"3\":\"\"}",
      "{\"1\":\"A\",\"-2.0\":\"\",\"3\":null}",
      "{\"1\":\"A\",\"-2.0\":null,\"3\":\"\"}",
      "{\"1\":\"A\",\"-2.0\":null,\"3\":null}");

    // When...
    content = "";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decodeObject( content)),
      "{}");

    // When...
    content = "-1.23";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));

    // When...
    content = "A,1,B,-2.0,C,3";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));

    // When...
    content = "A=1,B=-2.0,C==3";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));

    // When...
    content = "A=1,B,C=3";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));

    // When...
    content = "A=1,";
    
    // Then...
    assertJsonNodes( "Decoded", objectNodes( decoder.decodeObject( content)));
    }
  
  @Test
  public void whenArray()
    {
    // Given...
    boolean explode = false;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;

    // When...
    content = "A,B,C";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decodeArray( content),
      "[\"A\",\"B\",\"C\"]");

    // When...
    content = "1,,-2.34,";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      arrayNodes( decoder.decodeArray( content)),
      "[1,\"\",-2.34,\"\"]",
      "[1,\"\",-2.34,null]",
      "[1,\"\",\"-2.34\",\"\"]",
      "[1,\"\",\"-2.34\",null]",
      "[1,null,-2.34,\"\"]",
      "[1,null,-2.34,null]",
      "[1,null,\"-2.34\",\"\"]",
      "[1,null,\"-2.34\",null]",
      "[\"1\",\"\",-2.34,\"\"]",
      "[\"1\",\"\",-2.34,null]",
      "[\"1\",\"\",\"-2.34\",\"\"]",
      "[\"1\",\"\",\"-2.34\",null]",
      "[\"1\",null,-2.34,\"\"]",
      "[\"1\",null,-2.34,null]",
      "[\"1\",null,\"-2.34\",\"\"]",
      "[\"1\",null,\"-2.34\",null]");

    // When...
    content = "";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      arrayNodes( decoder.decodeArray( content)),
      "[]");

    // When...
    content = "-2.34";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      arrayNodes( decoder.decodeArray( content)),
      "[-2.34]",
      "[\"-2.34\"]");
    }

  private void assertJsonNodes( String label, List<JsonNode> actual, String... expected)
    {
    assertThat(
      label,
      actual.stream().map( String::valueOf).toArray( String[]::new),
      containsElements( expected));
    }

  private List<JsonNode> objectNodes( List<JsonNode> jsonNodes)
    {
    jsonNodes.stream()
      .forEach( node -> {
        assertThat( String.format( "'%s' is an object", node), node.isObject(), is( true));
        });

    return jsonNodes;
    }

  private List<JsonNode> arrayNodes( List<JsonNode> jsonNodes)
    {
    jsonNodes.stream()
      .forEach( node -> {
        assertThat( String.format( "'%s' is an array", node), node.isArray(), is( true));
        });

    return jsonNodes;
    }
  }
