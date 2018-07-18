//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Arrays;

/**
 * Builds {@link FunctionInputDef} instances.
 *
 */
public class FunctionInputDefBuilder
  {
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
      functionInputDef == null
      ? new FunctionInputDef( "F")
      : functionInputDef;

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
   * Adds a new {@link VarSet} with the given path name and returns a builder
   * for the new <CODE>VarSet</CODE>.
   */
  public VarSetBuilder varSetAtPath( String pathName)
    {
    return varSetAtPath( DefUtils.toPath( pathName));
    }

  /**
   * Adds a new {@link VarSet} with the given path name and returns a builder
   * for the new <CODE>VarSet</CODE>.
   */
  public VarSetBuilder varSetAtPath( String[] path)
    {
    VarSet varSet = null;
    if( path != null && path.length > 0)
      {
      varSet = new VarSet( path[0]);
      functionInputDef_.addVarDef( varSet);

      for( int i = 1; i < path.length; i++)
        {
        VarSet child = new VarSet( path[i]);
        varSet.addMember( child);
        varSet = child;
        }
      }

    return
      varSet == null
      ? null
      : new VarSetBuilder( varSet);
    }

  /**
   * Adds a new {@link VarDef} with the given path name and returns a builder
   * for the new <CODE>VarDef</CODE>.
   */
  public VarDefBuilder varDefAtPath( String pathName)
    {
    return varDefAtPath( DefUtils.toPath( pathName));
    }

  /**
   * Adds a new {@link VarDef} with the given path name and returns a builder
   * for the new <CODE>VarDef</CODE>.
   */
  public VarDefBuilder varDefAtPath( String[] path)
    {
    VarDefBuilder varDefBuilder = null;
    if( path != null && path.length > 0)
      {
      String varDefName = path[ path.length - 1];
      VarSetBuilder parentBuilder = varSetAtPath( Arrays.copyOfRange( path, 0, path.length - 1));
      if( parentBuilder != null)
        {
        varDefBuilder = parentBuilder.varDefAtPath( varDefName);
        }
      else
        {
        VarDef varDef = new VarDef( varDefName);
        functionInputDef_.addVarDef( varDef);
        varDefBuilder = new VarDefBuilder( varDef);
        }
      }

    return varDefBuilder;
    }

  FunctionInputDef functionInputDef_;
  }
