//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;

/**
 * Designates a "not applicable" condition for an input variable.
 *
 */
public class VarNaBinding extends VarBinding
  {
  /**
   * Creates a new VarNaBinding object.
   */
  public VarNaBinding( String varName)
    {
    this( varName, IVarDef.ARG);
    }

  /**
   * Creates a new VarNaBinding object.
   */
  public VarNaBinding( String varName, String varType)
    {
    super( varName, varType, null);
    }

  /**
   * Changes the value name for this binding.
   */
  public void setValue( String valueName)
    {
    if( valueName != null)
      {
      throw new UnsupportedOperationException( "Value name is undefined when variable is not applicable");
      }
    }

  /**
   * Changes if this variable is bound to a valid value.
   */
  @Override
public void setValueValid( boolean valid)
    {
    if( !valid)
      {
      throw new UnsupportedOperationException( "By definition, always valid for a variable to be not applicable");
      }
    }

  /**
   * Returns if this variable is bound to a valid value.
   */
  @Override
public boolean isValueValid()
    {
    return true;
    }

  /**
   * Returns true if this binding indicates a "not applicable" condition for this variable.
   */
  @Override
public boolean isValueNA()
    {
    return true;
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getVar())
      .toString();
    }
  }
