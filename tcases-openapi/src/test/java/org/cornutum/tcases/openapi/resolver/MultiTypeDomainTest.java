//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ValueDomain.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Optional;
import java.util.Random;

/**
 * Runs tests for {@link MultiTypeDomain}.
 */
public class MultiTypeDomainTest
  {
  @Test
  public void whenAllTypes()
    {
    // Given...
    long seed =
      Optional.ofNullable( System.getProperty( "seed"))
      .map( Long::valueOf)
      .orElse( new Random().nextLong());
    
    Random random = new Random( seed);
    MultiTypeDomain domain = new MultiTypeDomain( Type.any());

    // When...
    domain.values( random)
      .limit( domain.getTypes().length * 2)

      // Then...
      .forEach(
        value ->
        assertThat(
          String.format( "seed=%s, value=%s is member", seed, value),
          domain.contains( value),
          is( true)));
    }
  }
