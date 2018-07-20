//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;

/**
 * Builds {@link VarDef} instances.
 *
 */
public class VarDefBuilder
  {
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
   * Add a variable annotation.
   */
  public VarDefBuilder has( String name, String value)
    {
    varDef_.setAnnotation( name, value);
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

  VarDef varDef_;
  }

