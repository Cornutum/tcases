//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

/**
 * Builds {@link TupleCombiner} instances.
 *
 */
public class TupleCombinerBuilder
  {  
  /**
   * Creates a new builder for the given TupleCombiner.
   */
  public static TupleCombinerBuilder with( TupleCombiner tupleCombiner)
    {
    return new TupleCombinerBuilder( tupleCombiner);
    }


  /**
   * Creates a new TupleCombinerBuilder object.
   */
  public TupleCombinerBuilder()
    {
    start();
    }

  /**
   * Creates a new TupleCombinerBuilder object.
   */
  public TupleCombinerBuilder( TupleCombiner tupleCombiner)
    {
    start( tupleCombiner);
    }

  /**
   * Returns the current {@link TupleCombiner}.
   */
  public TupleCombiner build()
    {
    return tupleCombiner_;
    }

  /**
   * Starts building a new combiner.
   */
  public TupleCombinerBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new combiner.
   */
  public TupleCombinerBuilder start( TupleCombiner tupleCombiner)
    {
    tupleCombiner_ =
      tupleCombiner == null
      ? new TupleCombiner()
      : tupleCombiner;
    
    return this;
    }

  /**
   * Changes the tuple size for this combiner.
   */
  public TupleCombinerBuilder tuples( int tupleSize)
    {
    tupleCombiner_.setTupleSize( tupleSize);
    return this;
    }

  /**
   * Excludes the given variables from this combiner.
   */
  public TupleCombinerBuilder exclude( String... varNamePatterns)
    {
    for( String varNamePattern : varNamePatterns)
      {
      tupleCombiner_.addExcludedVar( varNamePattern);
      }
    return this;
    }

  /**
   * Includes the given variables in this combiner.
   */
  public TupleCombinerBuilder include( String... varNamePatterns)
    {
    for( String varNamePattern : varNamePatterns)
      {
      tupleCombiner_.addIncludedVar( varNamePattern);
      }
    return this;
    }

  /**
   * Adds the given once-only tuples to this combiner.
   */
  public TupleCombinerBuilder once( TupleRef... tupleRefs)
    {
    for( TupleRef tupleRef : tupleRefs)
      {
      tupleCombiner_.addOnceTuple( tupleRef);
      }
    return this;
    }

  TupleCombiner tupleCombiner_;
  }

