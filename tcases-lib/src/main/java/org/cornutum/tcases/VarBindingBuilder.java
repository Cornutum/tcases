//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Objects;
import java.util.Optional;

/**
 * Builds {@link VarBinding} instances.
 *
 */
public class VarBindingBuilder
  {
  /**
   * Creates a new builder for a VarBinding with the given variable name.
   */
  public static VarBindingBuilder with( String var)
    {
    return new VarBindingBuilder().var( var);
    }
  
  /**
   * Creates a new builder for the given VarBinding.
   */
  public static VarBindingBuilder with( VarBinding varBinding)
    {
    return new VarBindingBuilder( varBinding);
    }


  /**
   * Creates a new VarBindingBuilder object.
   */
  public VarBindingBuilder()
    {
    start();
    }

  /**
   * Creates a new VarBindingBuilder object.
   */
  public VarBindingBuilder( VarBinding binding)
    {
    start( binding);
    }

  /**
   * Returns the current binding.
   */
  public VarBinding build()
    {
    return varBinding_;
    }

  /**
   * Starts building a new binding.
   */
  public VarBindingBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new binding.
   */
  public VarBindingBuilder start( VarBinding binding)
    {
    varBinding_ =
      binding == null
      ? new VarBinding( "V", IVarDef.ARG, "?")
      : binding;
    
    return this;
    }

  /**
   * Changes the binding variable name.
   */
  public VarBindingBuilder var( String var)
    {
    varBinding_.setVar( var);
    return this;
    }

  /**
   * Changes the binding variable value.
   */
  public VarBindingBuilder value( Object value)
    {
    varBinding_.setValue( value);
    return this;
    }

  /**
   * Changes the binding variable type.
   */
  public VarBindingBuilder type( String type)
    {
    varBinding_.setType( type);
    return this;
    }

  /**
   * Changes if the binding value is valid.
   */
  public VarBindingBuilder valid( boolean valid)
    {
    varBinding_.setValueValid( valid);
    return this;
    }

  /**
   * Changes this binding to "not applicable".
   */
  public VarBindingBuilder notApplicable()
    {
    varBinding_ = new VarNaBinding( varBinding_.getVar(), varBinding_.getType());
    return this;
    }

  /**
   * Adds a binding annotation.
   */
  public VarBindingBuilder has( String name, Object value)
    {
    varBinding_.setAnnotation( name, Objects.toString( value, null));
    return this;
    }

  /**
   * Adds a binding annotation if the given value is non-null
   */
  public VarBindingBuilder hasIf( String name, Object value)
    {
    return
      value != null
      ? has( name, value)
      : this;
    }

  /**
   * Adds a binding annotation if the given value is defined
   */
  public VarBindingBuilder hasIf( String name, Optional<Object> value)
    {
    return hasIf( name, value.orElse( null));
    }

  VarBinding varBinding_;
  }

