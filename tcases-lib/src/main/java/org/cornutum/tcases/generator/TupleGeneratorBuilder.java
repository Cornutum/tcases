//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link TupleGenerator} instances.
 *
 */
public class TupleGeneratorBuilder
  {  
  /**
   * Creates a new builder for the given TupleGenerator.
   */
  public static TupleGeneratorBuilder with( TupleGenerator tupleGenerator)
    {
    return new TupleGeneratorBuilder( tupleGenerator);
    }


  /**
   * Creates a new TupleGeneratorBuilder object.
   */
  public TupleGeneratorBuilder()
    {
    start();
    }

  /**
   * Creates a new TupleGeneratorBuilder object.
   */
  public TupleGeneratorBuilder( TupleGenerator tupleGenerator)
    {
    start( tupleGenerator);
    }

  /**
   * Returns the current {@link TupleGenerator}.
   */
  public TupleGenerator build()
    {
    return tupleGenerator_;
    }

  /**
   * Starts building a new generator.
   */
  public TupleGeneratorBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new generator.
   */
  public TupleGeneratorBuilder start( TupleGenerator tupleGenerator)
    {
    tupleGenerator_ =
      Optional.ofNullable( tupleGenerator)
      .map( g -> (TupleGenerator) g.cloneOf())
      .orElse( new TupleGenerator());
    
    return this;
    }

  /**
   * Changes the default tuple size for this generator.
   */
  public TupleGeneratorBuilder tuples( int tupleSize)
    {
    tupleGenerator_.setDefaultTupleSize( tupleSize);
    return this;
    }

  /**
   * Changes the random number sequence seed for this generator.
   */
  public TupleGeneratorBuilder seed( Long seed)
    {
    tupleGenerator_.setRandomSeed( seed);
    return this;
    }

  /**
   * Changes the random number sequence seed for this generator.
   */
  public TupleGeneratorBuilder seed( int seed)
    {
    return seed( (long) seed);
    }

  /**
   * Add the given {@link TupleCombiner} instances to this generator.
   */
  public TupleGeneratorBuilder combining( TupleCombinerBuilder... combiners)
    {
    return combiners( Arrays.stream( combiners).map( TupleCombinerBuilder::build));
    }

  /**
   * Add the given {@link TupleCombiner} instances to this generator.
   */
  public TupleGeneratorBuilder combiners( TupleCombiner... combiners)
    {
    for( TupleCombiner combiner : combiners)
      {
      tupleGenerator_.addCombiner( combiner);
      }
    return this;
    }

  /**
   * Add the given {@link TupleCombiner} instances to this generator.
   */
  public TupleGeneratorBuilder combiners( Iterable<TupleCombiner> combiners)
    {
    for( TupleCombiner combiner : combiners)
      {
      tupleGenerator_.addCombiner( combiner);
      }
    return this;
    }

  /**
   * Add the given {@link TupleCombiner} instances to this generator.
   */
  public TupleGeneratorBuilder combiners( Stream<TupleCombiner> combiners)
    {
    combiners.forEach( combiner -> tupleGenerator_.addCombiner( combiner));
    return this;
    }

  TupleGenerator tupleGenerator_;
  }

