//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2023, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingBuilder;

import java.util.Arrays;

/**
 * Defines methods to build elements of a {@link GeneratorSet}.
 */
public final class Generators
  {
  /**
   * Creates a new Generators instance.
   */
  private Generators()
    {
    // Static methods only
    }

  /**
   * Returns a new {@link GeneratorSetBuilder}.
   */
  public static GeneratorSetBuilder generators()
    {
    return new GeneratorSetBuilder();
    }

  /**
   * Returns a new {@link TupleGeneratorBuilder} with the given default tuple size.
   */
  public static TupleGeneratorBuilder tuples( int tupleSize)
    {
    return new TupleGeneratorBuilder().tuples( tupleSize);
    }

  /**
   * Returns a new {@link TupleGeneratorBuilder} with default tuple size 1.
   */
  public static TupleGeneratorBuilder tuples()
    {
    return tuples( 1);
    }

  /**
   * Returns a new {@link TupleCombinerBuilder} with the given tuple size.
   */
  public static TupleCombinerBuilder tuplesOf( int tupleSize)
    {
    return new TupleCombinerBuilder().tuples( tupleSize);
    }

  /**
   * Returns a {@link TupleRefBuilder} for new "once" tuple.
   */
  public static TupleRefBuilder once( VarBindingBuilder... bindings)
    {
    return new TupleRefBuilder().bindings( Arrays.stream( bindings).map( VarBindingBuilder::build));
    }

  /**
   * Returns a {@link VarBindingBuilder} for the given variable.
   */
  public static VarBindingBuilder whenVar( String varPath)
    {
    return VarBindingBuilder.with( varPath);
    }

  }
