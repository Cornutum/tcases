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
    this( null);
    }
  
  /**
   * Creates a new SystemTestDefBuilder object.
   */
  public SystemTestDefBuilder( SystemTestDef systemTestDef)
    {
    start( systemTestDef);
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
    return start( null);
    }

  /**
   * Starts building a new system test definition.
   */
  public SystemTestDefBuilder start( SystemTestDef systemTestDef)
    {
    systemTestDef_ =
      systemTestDef == null
      ? new SystemTestDef( "S")
      : systemTestDef;
    
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
   * Adds system functions.
   */
  public SystemTestDefBuilder functions( FunctionTestDef... functions)
    {
    for( FunctionTestDef function : functions)
      {
      systemTestDef_.addFunctionTestDef( function);
      }
    return this;
    }

  SystemTestDef systemTestDef_;
  }
