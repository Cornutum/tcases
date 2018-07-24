//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

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
      tupleGenerator == null
      ? new TupleGenerator()
      : tupleGenerator;
    
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

  TupleGenerator tupleGenerator_;
  }

