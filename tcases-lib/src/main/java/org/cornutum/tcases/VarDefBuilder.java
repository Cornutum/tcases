//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;

import java.util.stream.Stream;

/**
 * Builds {@link VarDef} instances.
 *
 */
public class VarDefBuilder extends AnnotatedBuilder<VarDefBuilder>
  {
  /**
   * Creates a new builder for a VarDef with the given name.
   */
  public static VarDefBuilder with( String name)
    {
    return new VarDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given VarDef.
   */
  public static VarDefBuilder with( VarDef varDef)
    {
    return new VarDefBuilder( varDef);
    }


  /**
   * Creates a new VarDefBuilder object.
   */
  public VarDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new VarDefBuilder object.
   */
  public VarDefBuilder( VarDef varDef)
    {
    start( varDef);
    }

  /**
   * Returns the current VarDef definition.
   */
  public VarDef build()
    {
    return varDef_;
    }

  /**
   * Starts building a new VarDef definition.
   */
  public VarDefBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a VarDef definition.
   */
  public VarDefBuilder start( VarDef varDef)
    {
    varDef_ =
      varDef == null
      ? new VarDef( "V")
      : varDef;
    
    return this;
    }

  /**
   * Changes the variable name.
   */
  public VarDefBuilder name( String name)
    {
    varDef_.setName( name);
    return this;
    }

  /**
   * Changes the variable type.
   */
  public VarDefBuilder type( String type)
    {
    varDef_.setType( type);
    return this;
    }

  /**
   * Changes the variable condition.
   */
  public VarDefBuilder when( ICondition condition)
    {
    varDef_.setCondition( condition);
    return this;
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder values( VarValueDef... values)
    {
    for( VarValueDef value : values)
      {
      varDef_.addValue( value);
      }
    return this;
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder values( Iterable<VarValueDef> values)
    {
    for( VarValueDef value : values)
      {
      varDef_.addValue( value);
      }
    return this;
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder values( Stream<VarValueDef> values)
    {
    values.forEach( value -> varDef_.addValue( value));
    return this;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  protected Annotated getAnnotated()
    {
    return varDef_;
    }

  VarDef varDef_;
  }

