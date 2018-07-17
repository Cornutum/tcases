//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;


/**
 * Builds {@link SystemTestDef} instances.
 *
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
   * Returns the current system test definition.
   */
  public SystemTestDef build()
    {
    return systemTestDef_;
    }

  /**
   * Starts building a new system test definition.
   */
  public SystemTestDefBuilder start()
    {
    systemTestDef_ = new SystemTestDef();
    return this;
    }

  /**
   * Changes the system name.
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
