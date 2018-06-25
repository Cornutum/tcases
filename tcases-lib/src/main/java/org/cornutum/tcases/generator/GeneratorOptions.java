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
  }

