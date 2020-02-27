//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Optional;
import java.util.Random;

/**
 * Defines options used to resolve an executable API test case.
 */
public class ResolverOptions
  {
  /**
   * Creates a new ResolverOptions instance.
   */
  public ResolverOptions( Random random)
    {
    setRandom( random);
    }

  /**
   * Changes the random number generator used to resolve test cases.
   */
  public void setRandom( Random random)
    {
    random_ =
      Optional.ofNullable( random)
      .orElseThrow( () -> new IllegalStateException( "Random number generator must be defined"));
    }

  /**
   * Returns the random number generator used to resolve test cases.
   */
  public Random getRandom()
    {
    return random_;
    }

  /**
   * Returns a new ResolverOptions builder.
   */
  public static Builder builder( Random random)
    {
    return new Builder( random);
    }
  
  /**
   * Builds a new {@link ResolverOptions} instance.
   */
  public static class Builder
    {
    public Builder( Random random)
      {
      resolverOptions_ = new ResolverOptions( random);
      }

    public ResolverOptions build()
      {
      return resolverOptions_;
      }
      
    private ResolverOptions resolverOptions_;
    }

  private Random random_;
  }
