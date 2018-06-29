//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.validation.DefUtils;

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
    start();
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
    functionInputDef_ = new FunctionInputDef();
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
   * Adds a new {@link VarSet} with the given path name.
   */
  public VarSetBuilder varSet( String pathName)
    {
    return varSet( DefUtils.toPath( pathName));
    }

  /**
   * Adds a new {@link VarSet} with the given path name.
   */
  public VarSetBuilder varSet( String[] path)
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
   * Adds a new {@link VarDef} with the given path name.
   */
  public VarDefBuilder varDef( String pathName)
    {
    VarDefBuilder varDefBuilder = null;
    String[] path = DefUtils.toPath( pathName);
    if( path != null && path.length > 0)
      {
      String varDefName = path[ path.length - 1];
      VarSetBuilder parentBuilder = varSet( Arrays.copyOfRange( path, 0, path.length - 1));
      if( parentBuilder != null)
        {
        varDefBuilder = parentBuilder.varDef( varDefName);
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
