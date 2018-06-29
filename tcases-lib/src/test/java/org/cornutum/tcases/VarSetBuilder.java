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
 * Builds {@link VarSet} instances.
 *
 */
public class VarSetBuilder
  {
  /**
   * Creates a new VarSetBuilder object.
   */
  public VarSetBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new VarSetBuilder object.
   */
  public VarSetBuilder( VarSet varSet)
    {
    start( varSet);
    }

  /**
   * Returns the current VarSet definition.
   */
  public VarSet build()
    {
    return varSet_;
    }

  /**
   * Starts building a new VarSet definition.
   */
  public VarSetBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a VarSet definition.
   */
  private VarSetBuilder start( VarSet varSet)
    {
    varSet_ =
      varSet == null
      ? new VarSet()
      : varSet;
    
    return this;
    }

  /**
   * Changes the function name.
   */
  public VarSetBuilder name( String name)
    {
    varSet_.setName( name);
    return this;
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
      varSet_.addMember( varSet);

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
        varSet_.addMember( varDef);
        varDefBuilder = new VarDefBuilder( varDef);
        }
      }

    return varDefBuilder;
    }

  VarSet varSet_;
  }

