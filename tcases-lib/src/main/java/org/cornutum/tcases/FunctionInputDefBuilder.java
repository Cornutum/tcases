//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link FunctionInputDef} instances.
 *
 */
public class FunctionInputDefBuilder extends AnnotatedBuilder<FunctionInputDefBuilder>
  {
  /**
   * Creates a new builder for a FunctionInputDef with the given name.
   */
  public static FunctionInputDefBuilder with( String name)
    {
    return new FunctionInputDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given FunctionInputDef.
   */
  public static FunctionInputDefBuilder with( FunctionInputDef functionInputDef)
    {
    return new FunctionInputDefBuilder( functionInputDef);
    }

  /**
   * Creates a new FunctionInputDefBuilder object.
   */
  public FunctionInputDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new FunctionInputDefBuilder object.
   */
  public FunctionInputDefBuilder( FunctionInputDef functionInputDef)
    {
    start( functionInputDef);
    }

  /**
   * Returns the current function input definition.
   */
  public FunctionInputDef build()
    {
    return functionInputDef_;
    }

  /**
   * Starts building a new function input definition.
   */
  public FunctionInputDefBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new function input definition.
   */
  public FunctionInputDefBuilder start( FunctionInputDef functionInputDef)
    {
    functionInputDef_ =
      Optional.ofNullable( functionInputDef)
      .map( f ->
            FunctionInputDefBuilder.with( f.getName())
            .vars(
              toStream( f.getVarDefs())
              .map( v ->
                    v.getMembers() == null
                    ? (IVarDef) VarDefBuilder.with( (VarDef) v).build()
                    : (IVarDef) VarSetBuilder.with( (VarSet) v).build()))
            .annotations( f)
            .build())
      .orElse( new FunctionInputDef( "F"));

    return this;
    }

  /**
   * Changes the function name.
   */
  public FunctionInputDefBuilder name( String name)
    {
    functionInputDef_.setName( name);
    return this;
    }

  /**
   * Adds function input variables.
   */
  public FunctionInputDefBuilder vars( IVarDef... vars)
    {
    for( IVarDef var : vars)
      {
      functionInputDef_.addVarDef( var);
      }
    return this;
    }

  /**
   * Adds function input variables.
   */
  public FunctionInputDefBuilder vars( Iterable<IVarDef> vars)
    {
    for( IVarDef var : vars)
      {
      functionInputDef_.addVarDef( var);
      }
    return this;
    }

  /**
   * Adds function input variables.
   */
  public FunctionInputDefBuilder vars( Stream<IVarDef> vars)
    {
    vars.forEach( var -> functionInputDef_.addVarDef( var));
    return this;
    }

  /**
   * Adds function input variables of the given type.
   */
  public FunctionInputDefBuilder vars( String type, AbstractVarDef... vars)
    {
    for( AbstractVarDef var : vars)
      {
      var.setType( type);
      functionInputDef_.addVarDef( var);
      }
    return this;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
  protected Annotated getAnnotated()
    {
    return functionInputDef_;
    }

  FunctionInputDef functionInputDef_;
  }
