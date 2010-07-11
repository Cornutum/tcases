//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.VarDef;
import com.startingblocktech.tcases.VarValueDef;
import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.lang.ObjectUtils;

/**
 * Defines a selection of a {@link VarValueDef value definition} for an {@link VarDef input variable definition}.
 *
 * @version $Revision$, $Date$
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
    if( valueDef != null && varDef.getValue( valueDef.getName()) == null)
      {
      throw new IllegalArgumentException( "Value=" + valueDef + " is not defined for var=" + varDef);
      }
    
    varDef_ = varDef;
    valueDef_ = valueDef;

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

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ (getVarDef()==null? 0 : getVarDef().hashCode())
      ^ (getValueDef()==null? 0 : getValueDef().hashCode());
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "var", getVarDef())
      .append( "value", getValueDef())
      .toString();
    }

  private VarDef      varDef_;
  private VarValueDef valueDef_;
  }

