//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Optional;

/**
 * Runs tests for {@link Characters} implementations.
 */
public class CharactersTest
  {
  @Test
  public void anyTest()
    {
    assertThat( Characters.ANY.allowed( "{f81d4fae}{7dec}{11d0}{a765:00a0c91e6bf6}"), is( true));
    assertThat( Characters.ANY.allowed( "H81d4Hae-7dec-11d0-a765-00a0c91e6bH6"), is( true));
    assertThat( Characters.ANY.allowed( "What?! Me, worry?"), is( true));
    assertThat( Characters.ANY.allowed( "あ"), is( true));
    assertThat( Characters.ANY.allowed( "MañanaSchrödinger"), is( true));
    assertThat( Characters.ANY.allowed( "123456789"), is( true));
    assertThat( Characters.ANY.allowed( null), is( true));

    assertThat( Characters.ANY.filtered( null), is( Optional.empty()));
    assertThat( Characters.ANY.filtered( "").get(), is( ""));
    }
  
  @Test
  public void asciiTest()
    {
    assertThat( Characters.ASCII.allowed( "{f81d4fae}{7dec}{11d0}{a765:00a0c91e6bf6}"), is( true));
    assertThat( Characters.ASCII.allowed( "H81d4Hae-7dec-11d0-a765-00a0c91e6bH6"), is( true));
    assertThat( Characters.ASCII.allowed( "What?! Me, worry?"), is( true));
    assertThat( Characters.ASCII.allowed( "あ"), is( false));
    assertThat( Characters.ASCII.allowed( "MañanaSchrödinger"), is( false));
    assertThat( Characters.ASCII.allowed( "123456789"), is( true));

    assertThat( Characters.ASCII.filtered( "あ"), is( Optional.empty()));
    assertThat( Characters.ASCII.filtered( "MañanaSchrödinger").get(), is( "MaanaSchrdinger"));
    }
  
  @Test
  public void cookieValueTest()
    {
    assertThat( Characters.COOKIE_VALUE.allowed( "{f81d4fae}{7dec}{11d0}{a765:00a0c91e6bf6}"), is( true));
    assertThat( Characters.COOKIE_VALUE.allowed( "H81d4Hae-7dec-11d0-a765-00a0c91e6bH6"), is( true));
    assertThat( Characters.COOKIE_VALUE.allowed( "What?! Me, worry?"), is( false));
    assertThat( Characters.COOKIE_VALUE.allowed( "あ"), is( false));
    assertThat( Characters.COOKIE_VALUE.allowed( "MañanaSchrödinger"), is( false));
    assertThat( Characters.COOKIE_VALUE.allowed( "123456789"), is( true));

    assertThat( Characters.COOKIE_VALUE.filtered( "What?! Me, worry?").get(), is( "What?!Meworry?"));
    }
  
  @Test
  public void tokenTest()
    {
    assertThat( Characters.TOKEN.allowed( "{f81d4fae}{7dec}{11d0}{a765:00a0c91e6bf6}"), is( false));
    assertThat( Characters.TOKEN.allowed( "H81d4Hae-7dec-11d0-a765-00a0c91e6bH6"), is( true));
    assertThat( Characters.TOKEN.allowed( "What?! Me, worry?"), is( false));
    assertThat( Characters.TOKEN.allowed( "あ"), is( false));
    assertThat( Characters.TOKEN.allowed( "MañanaSchrödinger"), is( false));
    assertThat( Characters.TOKEN.allowed( "123456789"), is( true));

    assertThat( Characters.TOKEN.filtered( "{f81d4fae}{7dec}{11d0}{a765:00a0c91e6bf6}").get(), is( "f81d4fae7dec11d0a76500a0c91e6bf6"));
    }

  }
