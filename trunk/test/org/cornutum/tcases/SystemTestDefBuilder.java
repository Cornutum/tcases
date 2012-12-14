//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;


/**
 * Builds {@link SystemTestDef} instances.
 *
 * @version $Revision$, $Date$
 */
public class SystemTestDefBuilder
  {
  /**
   * Creates a new SystemTestDefBuilder object.
   */
  public SystemTestDefBuilder()
    {
    start();
    }

  /**
   * Returns the current function input definition.
   */
  public SystemTestDef build()
    {
    return systemTestDef_;
    }

  /**
   * Starts building a new function input definition.
   */
  public SystemTestDefBuilder start()
    {
    systemTestDef_ = new SystemTestDef();
    return this;
    }

  /**
   * Changes the function name.
   */
  public SystemTestDefBuilder name( String name)
    {
    systemTestDef_.setName( name);
    return this;
    }

  /**
   * Adds the given function test definition.
   */
  public FunctionTestDefBuilder function( FunctionTestDef function)
    {
    systemTestDef_.addFunctionTestDef( function);
    return new FunctionTestDefBuilder( function);
    }

  SystemTestDef systemTestDef_;
  }
