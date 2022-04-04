//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Optional;

/**
 * Runs {@link SimpleDecoder} tests.
 */
public class SimpleDecoderTest extends AbstractDecoderTest
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
  
  @Test
  public void whenValue()
    {
    // Given...
    boolean explode = false;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;

    // When...
    content = "1.234";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decodeValue( content),
      "1.234",
      "\"1.234\"");

    // When...
    content = "true";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      valueNodes( decoder.decodeValue( content)),
      "true",
      "\"true\"");

    // When...
    content = "";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      valueNodes( decoder.decodeValue( content)),
      "\"\"",
      "null");

    // When...
    content = "Murgatroid";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      valueNodes( decoder.decodeValue( content)),
      "\"Murgatroid\"");
    }
  
  @Test
  public void whenAny()
    {
    // Given...
    boolean explode = false;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;

    // When...
    content = "A,2";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decode( content),
      "{\"A\":2}",
      "{\"A\":\"2\"}",
      "[\"A\",2]",
      "[\"A\",\"2\"]",     
      "\"A,2\"");

    // When...
    content = "true";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decode( content),
      "[true]",
      "[\"true\"]",
      "true",
      "\"true\"");

    // When...
    content = "";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decode( content),
      "{}", 
      "[]", 
      "\"\"",
      "null");

    // When...
    content = "Murgatroid";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decode( content),
      "[\"Murgatroid\"]",
      "\"Murgatroid\"");
    }
  
  @Test
  public void whenBoolean()
    {
    // Given...
    boolean explode = false;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;
    Optional<JsonNode> jsonNode;
    
    // When...
    content = "true";
    
    // Then...
    jsonNode = decoder.decodeBoolean( content);
    assertThat( "Boolean content", jsonNode.isPresent(), is( true));
    assertThat( "BooleanNode", jsonNode.get().isBoolean(), is( true));
    assertThat( "Value", jsonNode.get().asBoolean(), is( true));

    // When...
    content = "false";
    
    // Then...
    jsonNode = decoder.decodeBoolean( content);
    assertThat( "Boolean content", jsonNode.isPresent(), is( true));
    assertThat( "BooleanNode", jsonNode.get().isBoolean(), is( true));
    assertThat( "Value", jsonNode.get().asBoolean(), is( false));

    // When...
    content = "?";
    
    // Then...
    jsonNode = decoder.decodeBoolean( content);
    assertThat( "Boolean content", jsonNode.isPresent(), is( false));

    // When...
    content = "FALSE";
    
    // Then...
    jsonNode = decoder.decodeBoolean( content);
    assertThat( "Boolean content", jsonNode.isPresent(), is( false));

    // When...
    content = null;
    
    // Then...
    jsonNode = decoder.decodeBoolean( content);
    assertThat( "Boolean content", jsonNode.isPresent(), is( false));

    // When...
    content = "-1.23";
    
    // Then...
    jsonNode = decoder.decodeBoolean( content);
    assertThat( "Boolean content", jsonNode.isPresent(), is( false));
    }
  
  @Test
  public void whenNumber()
    {
    // Given...
    boolean explode = false;
    SimpleDecoder decoder = new SimpleDecoder( explode);
    String content;
    Optional<JsonNode> jsonNode;
    
    // When...
    content = "-1.234";
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( true));
    assertThat( "DecimalNode", jsonNode.get().isBigDecimal(), is( true));
    assertThat( "Value", jsonNode.get().decimalValue(), is( new BigDecimal( content)));

    // When...
    content = "0.0";
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( true));
    assertThat( "DecimalNode", jsonNode.get().isBigDecimal(), is( true));
    assertThat( "Value", jsonNode.get().decimalValue(), is( new BigDecimal( content)));

    // When...
    content = "1234";
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( true));
    assertThat( "BigIntegerNode", jsonNode.get().isBigInteger(), is( true));
    assertThat( "Value", jsonNode.get().bigIntegerValue(), is( new BigInteger( content)));

    // When...
    content = "?";
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( false));

    // When...
    content = "false";
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( false));

    // When...
    content = null;
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( false));

    // When...
    content = "0XA";
    
    // Then...
    jsonNode = decoder.decodeNumber( content);
    assertThat( "Number content", jsonNode.isPresent(), is( false));
    }
  }
