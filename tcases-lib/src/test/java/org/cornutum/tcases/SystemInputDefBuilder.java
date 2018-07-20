//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

/**
 * Builds {@link SystemInputDef} instances.
 *
 */
public class SystemInputDefBuilder
  {
  /**
   * Creates a new SystemInputDefBuilder object.
   */
  public SystemInputDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemInputDefBuilder object.
   */
  public SystemInputDefBuilder( SystemInputDef systemInputDef)
    {
    start( systemInputDef);
    }

  /**
   * Returns the current system input definition.
   */
  public SystemInputDef build()
    {
    return systemInputDef_;
    }

  /**
   * Starts building a new system input definition.
   */
  public SystemInputDefBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new system input definition.
   */
  public SystemInputDefBuilder start( SystemInputDef systemInputDef)
    {
    systemInputDef_ =
      systemInputDef == null
      ? new SystemInputDef( "S")
      : systemInputDef;
    
    return this;
    }

  /**
   * Changes the system name.
   */
  public SystemInputDefBuilder name( String name)
    {
    systemInputDef_.setName( name);
    return this;
    }

  /**
   * Adds system functions.
   */
  public SystemInputDefBuilder functions( FunctionInputDef... functions)
    {
    for( FunctionInputDef function : functions)
      {
      systemInputDef_.addFunctionInputDef( function);
      }
    return this;
    }

  /**
   * Add a system annotation.
   */
  public SystemInputDefBuilder has( String name, String value)
    {
    systemInputDef_.setAnnotation( name, value);
    return this;
    }

  SystemInputDef systemInputDef_;
  }
