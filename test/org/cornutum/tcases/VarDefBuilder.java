//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

/**
 * Builds {@link VarDef} instances.
 *
 * @version $Revision$, $Date$
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
  private VarDefBuilder start( VarDef varDef)
    {
    varDef_ =
      varDef == null
      ? new VarDef()
      : varDef;
    
    return this;
    }

  /**
   * Changes the function name.
   */
  public VarDefBuilder name( String name)
    {
    varDef_.setName( name);
    return this;
    }

  VarDef varDef_;
  }

