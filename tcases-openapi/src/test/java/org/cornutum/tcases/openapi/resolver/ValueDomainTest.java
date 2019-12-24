//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import java.util.Optional;
import java.util.Random;
import static java.util.stream.Collectors.toList;

/**
 * Base class for {@link ValueDomain} tests.
 */
public abstract class ValueDomainTest
  {  
  /**
   * Returns the Random seed for this test.
   */
  protected long getSeed()
    {
    return seed_;
    }
  
  /**
   * Returns the Random instance for this test.
   */
  protected Random getRandom()
    {
    return random_;
    }

  /**
   * Generates up to the given limit of random values from the given domain, verifying that each of is contained by the domain.
   */
  protected <T> void verifyContainsValues( ValueDomain<T> domain, int limit)
    {
    List<T> values = 
      domain.values( getRandom())
      .limit( limit)
      .collect( toList());

    values.stream()
      .forEach(
        value ->
        assertThat(
          String.format( "seed=%s, value=%s is member", getSeed(), value),
          domain.contains( value),
          is( true)));
    }

  private static long seed_;
  private static Random random_;

  static
    {
    seed_ =
      Optional.ofNullable( System.getProperty( "seed"))
      .map( Long::valueOf)
      .orElse( new Random().nextLong());

    random_ = new Random( seed_);
    }
  }
