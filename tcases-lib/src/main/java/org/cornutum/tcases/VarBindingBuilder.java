//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Optional;

/**
 * Builds {@link VarBinding} instances.
 *
 */
public class VarBindingBuilder extends AnnotatedBuilder<VarBindingBuilder>
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
      Optional.ofNullable( binding)
      .map( b ->
            VarBindingBuilder.with( b.getVar())
            .value( b.getValue())
            .source( b.getSource())
            .type( b.getType())
            .valid( b.isValueValid())
            .notApplicable( b.isValueNA())
            .annotations( b)
            .build())
      .orElse( new VarBinding( "V", IVarDef.ARG, "?"));
    
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
  public VarBindingBuilder is( Object value)
    {
    return value( value);
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
   * Changes the binding value source.
   */
  public VarBindingBuilder source( Object source)
    {
    varBinding_.setSource( source);
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
   * Changes if this binding is "not applicable".
   */
  public VarBindingBuilder notApplicable( boolean na)
    {
    if( na)
      {
      varBinding_ = new VarNaBinding( varBinding_.getVar(), varBinding_.getType());
      }
    return this;
    }

  /**
   * Changes this binding to "not applicable".
   */
  public VarBindingBuilder isNA()
    {
    return notApplicable();
    }

  /**
   * Changes this binding to "not applicable".
   */
  public VarBindingBuilder notApplicable()
    {
    return notApplicable( true);
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
  protected Annotated getAnnotated()
    {
    return varBinding_;
    }

  VarBinding varBinding_;
  }

