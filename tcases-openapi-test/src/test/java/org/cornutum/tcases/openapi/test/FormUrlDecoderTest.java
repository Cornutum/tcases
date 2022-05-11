//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Runs {@link FormUrlDecoder} tests.
 */
public class FormUrlDecoderTest extends AbstractDecoderTest
  {
  @Test
  public void whenFormComposites()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "O", "X,-1,Y,1")
      .field( "A", "A,B,C")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"O\":{\"X\":-1,\"Y\":1},\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":{\"X\":-1,\"Y\":1},\"A\":\"A,B,C\"}",
      "{\"O\":{\"X\":-1,\"Y\":\"1\"},\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":{\"X\":-1,\"Y\":\"1\"},\"A\":\"A,B,C\"}",
      "{\"O\":{\"X\":\"-1\",\"Y\":1},\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":{\"X\":\"-1\",\"Y\":1},\"A\":\"A,B,C\"}",
      "{\"O\":{\"X\":\"-1\",\"Y\":\"1\"},\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":{\"X\":\"-1\",\"Y\":\"1\"},\"A\":\"A,B,C\"}",
      "{\"O\":[\"X\",-1,\"Y\",1],\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":[\"X\",-1,\"Y\",1],\"A\":\"A,B,C\"}",
      "{\"O\":[\"X\",-1,\"Y\",\"1\"],\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":[\"X\",-1,\"Y\",\"1\"],\"A\":\"A,B,C\"}",
      "{\"O\":[\"X\",\"-1\",\"Y\",1],\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":[\"X\",\"-1\",\"Y\",1],\"A\":\"A,B,C\"}",
      "{\"O\":[\"X\",\"-1\",\"Y\",\"1\"],\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":[\"X\",\"-1\",\"Y\",\"1\"],\"A\":\"A,B,C\"}",
      "{\"O\":\"X,-1,Y,1\",\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":\"X,-1,Y,1\",\"A\":\"A,B,C\"}");
    }
  
  @Test
  public void whenFormNumbers()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "N", "12.34")
      .field( "I", "1234")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"N\":[12.34],\"I\":[1234]}",
      "{\"N\":[12.34],\"I\":[\"1234\"]}",
      "{\"N\":[12.34],\"I\":1234}",
      "{\"N\":[12.34],\"I\":\"1234\"}",
      "{\"N\":[\"12.34\"],\"I\":[1234]}",
      "{\"N\":[\"12.34\"],\"I\":[\"1234\"]}",
      "{\"N\":[\"12.34\"],\"I\":1234}",
      "{\"N\":[\"12.34\"],\"I\":\"1234\"}",
      "{\"N\":12.34,\"I\":[1234]}",
      "{\"N\":12.34,\"I\":[\"1234\"]}",
      "{\"N\":12.34,\"I\":1234}",
      "{\"N\":12.34,\"I\":\"1234\"}",
      "{\"N\":\"12.34\",\"I\":[1234]}",
      "{\"N\":\"12.34\",\"I\":[\"1234\"]}",
      "{\"N\":\"12.34\",\"I\":1234}",
      "{\"N\":\"12.34\",\"I\":\"1234\"}");
    }
  
  @Test
  public void whenFormStrings()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "S", "What? Me worry?")
      .field( "?", "")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"S\":[\"What? Me worry?\"],\"?\":{}}",
      "{\"S\":[\"What? Me worry?\"],\"?\":[]}",
      "{\"S\":[\"What? Me worry?\"],\"?\":\"\"}",
      "{\"S\":[\"What? Me worry?\"],\"?\":null}",
      "{\"S\":\"What? Me worry?\",\"?\":{}}",
      "{\"S\":\"What? Me worry?\",\"?\":[]}",
      "{\"S\":\"What? Me worry?\",\"?\":\"\"}",
      "{\"S\":\"What? Me worry?\",\"?\":null}");
    }
  
  @Test
  public void whenFormBoolean()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "B", "true")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"B\":[true]}",
      "{\"B\":[\"true\"]}",
      "{\"B\":true}",
      "{\"B\":\"true\"}");
    }
  
  @Test
  public void whenFormEmpty()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content = "";
    
    // Then...
    assertJsonNodes(
      "Decoded",
      decoder.decode( content),
      "{}",
      "null");
    }
  
  @Test
  public void whenFormInvalid()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    expectFailure( IllegalStateException.class)
      .when( () -> decoder.decode( "12.34"))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "'12.34' is not a valid key/value pair"));
        });
    }

  @Test
  public void whenFormExploded()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .encodeProperty( "Pa", "pipeDelimited", true)
      .encodeProperty( "So", "spaceDelimited", true)
      .schema(
        "{"
        + "  \"type\": \"object\","
        + "  \"properties\": {"
        + "    \"O\": {"
        + "      \"type\": \"object\","
        + "      \"properties\": {"
        + "        \"X\": {"
        + "          \"type\": \"string\""
        + "        },"
        + "        \"Y\": {"
        + "          \"type\": \"string\""
        + "        },"
        + "        \"Z\": {"
        + "          \"type\": \"string\""
        + "        }"
        + "      }"
        + "    },"
        + "    \"A\": {"
        + "      \"type\": \"array\""
        + "    }"
        + "  }"
        + "}")
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "X", "f1")
      .field( "Y", "f2")
      .field( "A", "e0")
      .field( "A", "e1")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"O\":{\"X\":[\"f1\"],\"Y\":[\"f2\"]},\"A\":[[\"e0\"],[\"e1\"]]}",
      "{\"O\":{\"X\":[\"f1\"],\"Y\":[\"f2\"]},\"A\":[[\"e0\"],\"e1\"]}",
      "{\"O\":{\"X\":[\"f1\"],\"Y\":[\"f2\"]},\"A\":[\"e0\",[\"e1\"]]}",
      "{\"O\":{\"X\":[\"f1\"],\"Y\":[\"f2\"]},\"A\":[\"e0\",\"e1\"]}",

      "{\"O\":{\"X\":[\"f1\"],\"Y\":\"f2\"},\"A\":[[\"e0\"],[\"e1\"]]}",
      "{\"O\":{\"X\":[\"f1\"],\"Y\":\"f2\"},\"A\":[[\"e0\"],\"e1\"]}",
      "{\"O\":{\"X\":[\"f1\"],\"Y\":\"f2\"},\"A\":[\"e0\",[\"e1\"]]}",
      "{\"O\":{\"X\":[\"f1\"],\"Y\":\"f2\"},\"A\":[\"e0\",\"e1\"]}",

      "{\"O\":{\"X\":\"f1\",\"Y\":[\"f2\"]},\"A\":[[\"e0\"],[\"e1\"]]}",
      "{\"O\":{\"X\":\"f1\",\"Y\":[\"f2\"]},\"A\":[[\"e0\"],\"e1\"]}",
      "{\"O\":{\"X\":\"f1\",\"Y\":[\"f2\"]},\"A\":[\"e0\",[\"e1\"]]}",
      "{\"O\":{\"X\":\"f1\",\"Y\":[\"f2\"]},\"A\":[\"e0\",\"e1\"]}",

      "{\"O\":{\"X\":\"f1\",\"Y\":\"f2\"},\"A\":[[\"e0\"],[\"e1\"]]}",
      "{\"O\":{\"X\":\"f1\",\"Y\":\"f2\"},\"A\":[[\"e0\"],\"e1\"]}",
      "{\"O\":{\"X\":\"f1\",\"Y\":\"f2\"},\"A\":[\"e0\",[\"e1\"]]}",
      "{\"O\":{\"X\":\"f1\",\"Y\":\"f2\"},\"A\":[\"e0\",\"e1\"]}");

    // When...
    content =
      FormUrlBuilder.with()
      .field( "Pa", "x|vx")
      .field( "Pa", "y|vy")
      .field( "So", "x vx")
      .field( "So", "y vy")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Pa\":[[\"x|vx\"],[\"y|vy\"]],\"So\":[[\"x vx\"],[\"y vy\"]]}",
      "{\"Pa\":[[\"x|vx\"],[\"y|vy\"]],\"So\":[[\"x vx\"],\"y vy\"]}",
      "{\"Pa\":[[\"x|vx\"],[\"y|vy\"]],\"So\":[\"x vx\",[\"y vy\"]]}",
      "{\"Pa\":[[\"x|vx\"],[\"y|vy\"]],\"So\":[\"x vx\",\"y vy\"]}",

      "{\"Pa\":[[\"x|vx\"],\"y|vy\"],\"So\":[[\"x vx\"],[\"y vy\"]]}",
      "{\"Pa\":[[\"x|vx\"],\"y|vy\"],\"So\":[[\"x vx\"],\"y vy\"]}",
      "{\"Pa\":[[\"x|vx\"],\"y|vy\"],\"So\":[\"x vx\",[\"y vy\"]]}",
      "{\"Pa\":[[\"x|vx\"],\"y|vy\"],\"So\":[\"x vx\",\"y vy\"]}",

      "{\"Pa\":[\"x|vx\",[\"y|vy\"]],\"So\":[[\"x vx\"],[\"y vy\"]]}",
      "{\"Pa\":[\"x|vx\",[\"y|vy\"]],\"So\":[[\"x vx\"],\"y vy\"]}",
      "{\"Pa\":[\"x|vx\",[\"y|vy\"]],\"So\":[\"x vx\",[\"y vy\"]]}",

      "{\"Pa\":[\"x|vx\",[\"y|vy\"]],\"So\":[\"x vx\",\"y vy\"]}",
      "{\"Pa\":[\"x|vx\",\"y|vy\"],\"So\":[[\"x vx\"],[\"y vy\"]]}",
      "{\"Pa\":[\"x|vx\",\"y|vy\"],\"So\":[[\"x vx\"],\"y vy\"]}",
      "{\"Pa\":[\"x|vx\",\"y|vy\"],\"So\":[\"x vx\",[\"y vy\"]]}",
      "{\"Pa\":[\"x|vx\",\"y|vy\"],\"So\":[\"x vx\",\"y vy\"]}");
    }

  @Test
  public void whenFormDeep()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .encodeProperty( "Do", "deepObject", true)
      .encodeProperty( "Uo", "deepObject", false)
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .deepField( "Do", "X", "0")
      .deepField( "Do", "Y", "1")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Do\":{\"X\":[0],\"Y\":[1]}}",
      "{\"Do\":{\"X\":[0],\"Y\":[\"1\"]}}",
      "{\"Do\":{\"X\":[0],\"Y\":1}}",
      "{\"Do\":{\"X\":[0],\"Y\":\"1\"}}",

      "{\"Do\":{\"X\":[\"0\"],\"Y\":[1]}}",
      "{\"Do\":{\"X\":[\"0\"],\"Y\":[\"1\"]}}",
      "{\"Do\":{\"X\":[\"0\"],\"Y\":1}}",
      "{\"Do\":{\"X\":[\"0\"],\"Y\":\"1\"}}",

      "{\"Do\":{\"X\":0,\"Y\":[1]}}",
      "{\"Do\":{\"X\":0,\"Y\":[\"1\"]}}",
      "{\"Do\":{\"X\":0,\"Y\":1}}",
      "{\"Do\":{\"X\":0,\"Y\":\"1\"}}",

      "{\"Do\":{\"X\":\"0\",\"Y\":[1]}}",
      "{\"Do\":{\"X\":\"0\",\"Y\":[\"1\"]}}",
      "{\"Do\":{\"X\":\"0\",\"Y\":1}}",
      "{\"Do\":{\"X\":\"0\",\"Y\":\"1\"}}");

    // Then...
    expectFailure( IllegalStateException.class)
      .when( () -> {
        decoder.decode(
          FormUrlBuilder.with()
          .deepField( "Uo", "X", "Y")
          .build());
        })
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Unexpected value for exploded property=Key[Uo,X]"));
        });
    }

  @Test
  public void whenFormUnexploded()
    {
    // Given...
    ContentDef contentDef =
      ContentDefBuilder.forType( "application/x-www-form-urlencoded")
      .encodeProperty( "Pa", "pipeDelimited")
      .encodeProperty( "Sa", "spaceDelimited")
      .encodeProperty( "Ua", false)
      .encodeProperty( "Po", "pipeDelimited")
      .encodeProperty( "So", "spaceDelimited")
      .encodeProperty( "Uo", false)
      .build();

    FormUrlDecoder decoder = new FormUrlDecoder( contentDef);

    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "Pa", "0|1|2")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Pa\":[0,1,2]}",
      "{\"Pa\":[0,1,\"2\"]}",
      "{\"Pa\":[0,\"1\",2]}",
      "{\"Pa\":[0,\"1\",\"2\"]}",
      "{\"Pa\":[\"0\",1,2]}",
      "{\"Pa\":[\"0\",1,\"2\"]}",
      "{\"Pa\":[\"0\",\"1\",2]}",
      "{\"Pa\":[\"0\",\"1\",\"2\"]}",
      "{\"Pa\":\"0|1|2\"}");

    // When...
    content =
      FormUrlBuilder.with()
      .field( "Sa", "0 1 2")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Sa\":[0,1,2]}",
      "{\"Sa\":[0,1,\"2\"]}",
      "{\"Sa\":[0,\"1\",2]}",
      "{\"Sa\":[0,\"1\",\"2\"]}",
      "{\"Sa\":[\"0\",1,2]}",
      "{\"Sa\":[\"0\",1,\"2\"]}",
      "{\"Sa\":[\"0\",\"1\",2]}",
      "{\"Sa\":[\"0\",\"1\",\"2\"]}",
      "{\"Sa\":\"0 1 2\"}");

    // When...
    content =
      FormUrlBuilder.with()
      .field( "Ua", "0,1,2")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Ua\":[0,1,2]}",
      "{\"Ua\":[0,1,\"2\"]}",
      "{\"Ua\":[0,\"1\",2]}",
      "{\"Ua\":[0,\"1\",\"2\"]}",
      "{\"Ua\":[\"0\",1,2]}",
      "{\"Ua\":[\"0\",1,\"2\"]}",
      "{\"Ua\":[\"0\",\"1\",2]}",
      "{\"Ua\":[\"0\",\"1\",\"2\"]}",
      "{\"Ua\":\"0,1,2\"}");

    // When...
    content =
      FormUrlBuilder.with()
      .field( "Po", "X|x|Y|")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Po\":{\"X\":\"x\",\"Y\":\"\"}}",
      "{\"Po\":{\"X\":\"x\",\"Y\":null}}",
      "{\"Po\":[\"X\",\"x\",\"Y\",\"\"]}",
      "{\"Po\":[\"X\",\"x\",\"Y\",null]}",
      "{\"Po\":\"X|x|Y|\"}");

    // When...
    content =
      FormUrlBuilder.with()
      .field( "So", "X x Y ")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"So\":{\"X\":\"x\",\"Y\":\"\"}}",
      "{\"So\":{\"X\":\"x\",\"Y\":null}}",
      "{\"So\":[\"X\",\"x\",\"Y\",\"\"]}",
      "{\"So\":[\"X\",\"x\",\"Y\",null]}",
      "{\"So\":\"X x Y \"}");

    // When...
    content =
      FormUrlBuilder.with()
      .field( "Uo", "X,x,Y,")
      .build();
    
    // Then...
    assertJsonNodes(
      "Decoded",
      objectNodes( decoder.decode( content)),
      "{\"Uo\":{\"X\":\"x\",\"Y\":\"\"}}",
      "{\"Uo\":{\"X\":\"x\",\"Y\":null}}",
      "{\"Uo\":[\"X\",\"x\",\"Y\",\"\"]}",
      "{\"Uo\":[\"X\",\"x\",\"Y\",null]}",
      "{\"Uo\":\"X,x,Y,\"}");
    
    // Then...
    expectFailure( IllegalStateException.class)
      .when( () -> {
        decoder.decode(
          FormUrlBuilder.with()
          .deepField( "Uo", "X", "x")
          .deepField( "Uo", "Y", "")
          .build());
        })
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Expected explode=false for property='Uo' but found 2 bindings"));
        });
    }
  }
