//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.AllOf;
import org.cornutum.tcases.conditions.ICondition;

import org.apache.commons.lang3.ObjectUtils;

/**
 * Defines a selection of a {@link VarValueDef value definition} for an {@link VarDef input variable definition}.
 *
 */
public class VarBindingDef
  {
  /**
   * Creates a new VarBindingDef object.
   */
  public VarBindingDef()
    {
    this( null, null);
    }
  
  /**
   * Creates a new VarBindingDef object.
   */
  public VarBindingDef( VarDef varDef)
    {
    this( varDef, null);
    }
  
  /**
   * Creates a new VarBindingDef object.
   */
  public VarBindingDef( VarDef varDef, VarValueDef valueDef)
    {
    bind( varDef, valueDef);
    }

  /**
   * Changes this input variable binding.
   */
  public VarBindingDef bind( VarDef varDef, VarValueDef valueDef)
    {
    if( valueDef != null && !varDef.isApplicable( valueDef))
      {
      throw new IllegalArgumentException( "Value=" + valueDef + " is not defined for var=" + varDef);
      }
    
    varDef_ = varDef;
    valueDef_ = valueDef;
    effCondition_ = null;

    return this;
    }

  /**
   * Returns the input variable definition for this binding.
   */
  public VarDef getVarDef()
    {
    return varDef_;
    }

  /**
   * Returns the value definition for this binding.
   */
  public VarValueDef getValueDef()
    {
    return valueDef_;
    }

  /**
   * Returns the effective condition that defines when this binding is applicable.
   */
  public ICondition getEffectiveCondition()
    {
    if( effCondition_ == null)
      {
      ICondition  varCondition    = getVarDef().getEffectiveCondition();
      ICondition  valueCondition  = getValueDef().getCondition();

      effCondition_ =
        valueCondition == null
        ? varCondition 
        : new AllOf( varCondition, valueCondition);
      }
    
    return effCondition_;
    }

  /**
   * Returns true if this binding has the "not applicable" value.
   */
  public boolean isNA()
    {
    return VarValueDef.isNA( valueDef_);
    }

  @SuppressWarnings("deprecation")
  public boolean equals( Object object)
    {
    VarBindingDef other =
      object != null && object.getClass().equals( getClass())
      ? (VarBindingDef) object
      : null;

    return
      other != null
      && ObjectUtils.equals( other.getVarDef(), getVarDef())
      && ObjectUtils.equals( other.getValueDef(), getValueDef());
    }

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( getVarDef())
      ^ ObjectUtils.hashCode( getValueDef());
    }

  public String toString()
    {
    return
      new StringBuilder()
      .append( getVarDef()==null? ":" : getVarDef().getName())
      .append( '=')
      .append( getValueDef()==null? ":" : getValueDef().getName())
      .toString();
    }

  private VarDef      varDef_;
  private VarValueDef valueDef_;
  private ICondition  effCondition_;
  }

