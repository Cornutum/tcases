//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.VarBindingBuilder;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link TupleRef} instances.
 *
 */
public class TupleRefBuilder
  {
  /**
   * Creates a new builder with the given bindings.
   */
  public static TupleRefBuilder with( VarBinding... bindings)
    {
    return new TupleRefBuilder().bindings( bindings);
    }

  /**
   * Creates a new builder for the given TupleRef.
   */
  public static TupleRefBuilder with( TupleRef tupleRef)
    {
    return new TupleRefBuilder( tupleRef);
    }

  /**
   * Creates a new TupleRefBuilder object.
   */
  public TupleRefBuilder()
    {
    start();
    }

  /**
   * Creates a new TupleRefBuilder object.
   */
  public TupleRefBuilder( TupleRef tupleRef)
    {
    start( tupleRef);
    }

  /**
   * Returns the current {@link TupleRef}.
   */
  public TupleRef build()
    {
    return tupleRef_;
    }

  /**
   * Starts building a new tuple.
   */
  public TupleRefBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new tuple.
   */
  public TupleRefBuilder start( TupleRef tupleRef)
    {
    tupleRef_ =
      Optional.ofNullable( tupleRef)
      .map( tr ->
            new TupleRefBuilder()
            .bindings(
              toStream( tr.getVarBindings())
              .map( b -> VarBindingBuilder.with( b).build()))
            .build())
      .orElse( new TupleRef());
    
    return this;
    }

  /**
   * Adds the given bindings to this tuple.
   */
  public TupleRefBuilder bindings( VarBinding... bindings)
    {
    for( VarBinding binding : bindings)
      {
      tupleRef_.addVarBinding( binding);
      }
    return this;
    }

  /**
   * Adds the given bindings to this tuple.
   */
  public TupleRefBuilder bindings( Iterable<VarBinding> bindings)
    {
    for( VarBinding binding : bindings)
      {
      tupleRef_.addVarBinding( binding);
      }
    return this;
    }

  /**
   * Adds the given bindings to this tuple.
   */
  public TupleRefBuilder bindings( Stream<VarBinding> bindings)
    {
    bindings.forEach( binding -> tupleRef_.addVarBinding( binding));
    return this;
    }

  /**
   * Adds the given binding to this tuple.
   */
  public TupleRefBuilder bind( String var, String value)
    {
    tupleRef_.addVarBinding( new VarBinding( var, value));
    return this;
    }

  TupleRef tupleRef_;
  }

