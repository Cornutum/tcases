//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.VarBindingBuilder;
import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;

import org.junit.Test;

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
      .mapToObj( i -> { byte[] b = new byte[4]; Arrays.fill( b, (byte) i); return b; })
      .map( b -> Base64Domain.encoded( b))
      .collect( toSet()));

    // Then...
    verifyContainsValues( domain, 10);
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
    }
  }
