//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link SystemTestDef} instances.
 *
 */
public class SystemTestDefBuilder
  {
  /**
   * Creates a new builder for a SystemTestDef with the given name.
   */
  public static SystemTestDefBuilder with( String name)
    {
    return new SystemTestDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given SystemTestDef.
   */
  public static SystemTestDefBuilder with( SystemTestDef systemTestDef)
    {
    return new SystemTestDefBuilder( systemTestDef);
    }


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

  /**
   * Adds system functions.
   */
  public SystemTestDefBuilder functions( Iterable<FunctionTestDef> functions)
    {
    for( FunctionTestDef function : functions)
      {
      systemTestDef_.addFunctionTestDef( function);
      }
    return this;
    }

  /**
   * Adds system functions.
   */
  public SystemTestDefBuilder functions( Stream<FunctionTestDef> functions)
    {
    functions.forEach( function -> systemTestDef_.addFunctionTestDef( function));
    return this;
    }

  /**
   * Adds a system annotation.
   */
  public SystemTestDefBuilder has( String name, Object value)
    {
    systemTestDef_.setAnnotation( name, Objects.toString( value, null));
    return this;
    }

  /**
   * Adds a system annotation if the given value is non-null
   */
  public SystemTestDefBuilder hasIf( String name, Object value)
    {
    return
      value != null
      ? has( name, value)
      : this;
    }

  /**
   * Adds a system annotation if the given value is defined
   */
  public SystemTestDefBuilder hasIf( String name, Optional<Object> value)
    {
    return hasIf( name, value.orElse( null));
    }

  SystemTestDef systemTestDef_;
  }
