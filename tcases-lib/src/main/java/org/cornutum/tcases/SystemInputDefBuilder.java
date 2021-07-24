//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.stream.Stream;

/**
 * Builds {@link SystemInputDef} instances.
 *
 */
public class SystemInputDefBuilder extends AnnotatedBuilder<SystemInputDefBuilder>
  {
  /**
   * Creates a new builder for a SystemInputDef with the given name.
   */
  public static SystemInputDefBuilder with( String name)
    {
    return new SystemInputDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given SystemInputDef.
   */
  public static SystemInputDefBuilder with( SystemInputDef systemInputDef)
    {
    return new SystemInputDefBuilder( systemInputDef);
    }


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
   * Adds system functions.
   */
  public SystemInputDefBuilder functions( Iterable<FunctionInputDef> functions)
    {
    for( FunctionInputDef function : functions)
      {
      systemInputDef_.addFunctionInputDef( function);
      }
    return this;
    }

  /**
   * Adds system functions.
   */
  public SystemInputDefBuilder functions( Stream<FunctionInputDef> functions)
    {
    functions.forEach( function -> systemInputDef_.addFunctionInputDef( function));
    return this;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
  protected Annotated getAnnotated()
    {
    return systemInputDef_;
    }

  SystemInputDef systemInputDef_;
  }
