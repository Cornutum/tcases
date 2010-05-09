//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;
import static com.startingblocktech.tcases.DefUtils.*;

import org.apache.commons.lang.builder.ToStringBuilder;

/**
 * Defines the binding of an input variable to a specific value.
 *
 * @version $Revision$, $Date$
 */
public class VarBinding
  {
  /**
   * Creates a new VarBinding object.
   */
  public VarBinding()
    {
    this( null, null, null);
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public VarBinding( String varName, String valueName)
    {
    this( varName, IVarDef.ARG, valueName);
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public VarBinding( String varName, String varType, String valueName)
    {
    setVar( varName);
    setType( varType);
    setValue( valueName);
    setValueValid( true);
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public VarBinding( IVarDef varDef, VarValueDef valueDef)
    {
    this( varDef.getPathName(), varDef.getType(), valueDef.getName());
    setValueValid( valueDef.getType().isValid());
    }

  /**
   * Changes the variable name for this binding.
   */
  public void setVar( String varName)
    {
    assertIdentifier( varName);
    var_ = varName;
    }

  /**
   * Returns the variable name for this binding.
   */
  public String getVar()
    {
    return var_;
    }

  /**
   * Changes the value name for this binding.
   */
  public void setValue( String valueName)
    {
    assertIdentifier( valueName);
    value_ = valueName;
    }

  /**
   * Returns the value name for this binding.
   */
  public String getValue()
    {
    return value_;
    }

  /**
   * Changes the type identifier for this variable.
   */
  public void setType( String type)
    {
    assertIdentifier( type);
    varType_ = type;
    }

  /**
   * Returns the type identifier for this variable.
   */
  public String getType()
    {
    return varType_;
    }

  /**
   * Changes if this variable is bound to a valid value.
   */
  public void setValueValid( boolean valid)
    {
    valueValid_ = valid;
    }

  /**
   * Returns if this variable is bound to a valid value.
   */
  public boolean isValueValid()
    {
    return valueValid_;
    }

  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    builder.getStringBuffer()
      .append( getVar())
      .append( '=')
      .append( getValue());

    if( !isValueValid())
      {
      builder.append( "valid", isValueValid());
      }

    return builder.toString();
    }

  private String var_;
  private String varType_;
  private String value_;
  private boolean valueValid_;
  }

