//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

/**
 * Builds {@link FunctionTestDef} instances.
 *
 * @version $Revision$, $Date$
 */
public class FunctionTestDefBuilder
  {
  /**
   * Creates a new FunctionTestDefBuilder object.
   */
  public FunctionTestDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new FunctionTestDefBuilder object.
   */
  public FunctionTestDefBuilder( FunctionTestDef function)
    {
    start( function);
    }

  /**
   * Returns the current function test definition.
   */
  public FunctionTestDef build()
    {
    return functionTestDef_;
    }

  /**
   * Starts building a new function test definition.
   */
  public FunctionTestDefBuilder start( FunctionTestDef function)
    {
    functionTestDef_ =
      function == null
      ? new FunctionTestDef()
      : function;
    return this;
    }

  /**
   * Starts building a new function test definition.
   */
  public FunctionTestDefBuilder start()
    {
    return start( null);
    }

  /**
   * Changes the function name.
   */
  public FunctionTestDefBuilder name( String name)
    {
    functionTestDef_.setName( name);
    return this;
    }

  FunctionTestDef functionTestDef_;
  }
