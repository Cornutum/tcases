//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.VarBindingBuilder;
import org.cornutum.tcases.resolve.NumberDomain.Range;
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.stream.IntStream;
import static java.util.stream.Collectors.toSet;

/**
 * Runs tests for {@link Base64Domain}.
 */
public class Base64DomainTest extends ValueDomainTest
  {
  @Test
  public void whenLengthBelowMax()
    {
    // Given...
    Base64Domain domain = new Base64Domain( 200);

    // When...
    domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "< 100").build()));

    // Then...
    verifyContainsValues( domain, 10);

    assertThat( "Contains", domain.contains( encoded( toBytes())), is( true));
    assertThat( "Contains", domain.contains( encoded( fillBytes( 99, 255))), is( true));
    assertThat( "Contains", domain.contains( encoded( fillBytes( 100, 255))), is( false));
    }

  @Test
  public void whenExcluded()
    {
    // Given...
    Base64Domain domain = new Base64Domain( 200);

    // When...
    domain.setLengthRange( 4);
    domain.setExcluded(
      IntStream.range( 0, 256)
      .mapToObj( i -> encoded( fillBytes( 4, i)))
      .collect( toSet()));

    // Then...
    verifyContainsValues( domain, 10);

    assertThat( "Contains", domain.contains( encoded( toBytes( 1, 2, 3, 4))), is( true));
    assertThat( "Contains", domain.contains( encoded( toBytes( 1, 2, 3))), is( false));
    assertThat( "Contains", domain.contains( encoded( toBytes( 0, 0, 0, 0))), is( false));
    }

  @Test
  public void whenAboveMax()
    {
    // Given...
    Base64Domain domain = new Base64Domain( 8);

    // When...
    domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "> 4").build()));

    // Then...
    verifyContainsValues( domain, 10);

    assertThat( "Contains", domain.contains( encoded( toBytes( 1, 2, 3, 4, 5))), is( true));
    assertThat( "Contains", domain.contains( encoded( toBytes( 1, 2, 3, 4))), is( false));
    assertThat( "Contains", domain.contains( "You call this base64?"), is( false));
    }

  /**
   * Returns the given byte array.
   */
  private byte[] toBytes( int... values)
    {
    byte[] bytes = new byte[ values.length];
    for( int i = 0; i < bytes.length; i++)
      {
      bytes[i] = (byte) values[i];
      }

    return bytes;
    }

  /**
   * Returns a byte array contain the given number of copies of the given byte value.
   */
  private byte[] fillBytes( int length, int value)
    {
    byte[] bytes = new byte[length];
    Arrays.fill( bytes, (byte) value);
    return bytes;
    }

  /**
   * Returns the base64 encoding of the given byte array.
   */
  private String encoded( byte[] values)
    {
    return Base64Domain.encoded( values);
    }

  }
