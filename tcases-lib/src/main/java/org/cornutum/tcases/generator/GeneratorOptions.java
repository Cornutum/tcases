//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.util.ToString;

/**
 * Defines options that control test case generation.
 *
 */
public class GeneratorOptions 
  {
  /**
   * Changes the random seed used by generators.
   */
  public void setRandomSeed( Long seed)
    {
    seed_ = seed;
    }

  /**
   * Returns the random seed used by generators.
   */
  public Long getRandomSeed()
    {
    return seed_;
    }

  /**
   * Changes the default tuple size used by generators.
   */
  public void setDefaultTupleSize( Integer tupleSize)
    {
    defaultTupleSize_ = tupleSize;
    }

  /**
   * Returns the default tuple size used by generators.
   */
  public Integer getDefaultTupleSize()
    {
    return defaultTupleSize_;
    }    

  /**
   * Returns a new GeneratorOptions builder.
   */
  public static Builder builder()
    {
    return new Builder();
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "seed", getRandomSeed())
      .append( "tuples", getDefaultTupleSize())
      .build();
    }

  private Long seed_;
  private Integer defaultTupleSize_;    

  public static class Builder
    {
    public Builder()
      {
      options_ = new GeneratorOptions();
      }

    public Builder seed( Long seed)
      {
      options_.setRandomSeed( seed);
      return this;
      }

    public Builder tuples( int tuples)
      {
      options_.setDefaultTupleSize( tuples);
      return this;
      } 

    public GeneratorOptions build()
      {
      return options_;
      }
      
    private GeneratorOptions options_;
    }
  }

