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
  public void whenFormExploded()
    {
    // Given...
    boolean explode = true;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);
    String content;

    // When...
    content =
      FormUrlBuilder.with()
      .field( "O", "X=-1,Y=1")
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
      "{\"O\":[\"X=-1\",\"Y=1\"],\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":[\"X=-1\",\"Y=1\"],\"A\":\"A,B,C\"}",
      "{\"O\":\"X=-1,Y=1\",\"A\":[\"A\",\"B\",\"C\"]}",
      "{\"O\":\"X=-1,Y=1\",\"A\":\"A,B,C\"}");
    }
  
  @Test
  public void whenFormUnexploded()
    {
    // Given...
    boolean explode = false;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);
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
    boolean explode = false;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);
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
    boolean explode = false;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);
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
    boolean explode = false;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);
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
    boolean explode = false;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);
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
    boolean explode = true;
    FormUrlDecoder decoder = new FormUrlDecoder( explode);

    expectFailure( IllegalArgumentException.class)
      .when( () -> decoder.decode( "12.34"))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "'12.34' is not a valid key/value pair"));
        });
    }
  }
