//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingDef;

/**
 * Base class for exceptions that occur when binding input variables.
 *
 */
public abstract class BindingException extends Exception
  {
  /**
   * Creates a new BindingException object.
   */
  protected BindingException( VarBindingDef binding)
    {
    setBinding( binding);
    }

  /**
   * Changes the binding that is in error.
   */
  private void setBinding( VarBindingDef binding)
    {
    binding_ = binding;
    }

  /**
   * Returns the binding that is in error.
   */
  public VarBindingDef getBinding()
    {
    return binding_;
    }

  private VarBindingDef binding_;
  private static final long serialVersionUID = 8179656208405791203L;
  }
