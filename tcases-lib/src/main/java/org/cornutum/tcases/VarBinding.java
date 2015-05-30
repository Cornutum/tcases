//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.DefUtils.*;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Defines the binding of an input variable to a specific value.
 *
 */
public class VarBinding extends Annotated implements Comparable<VarBinding>
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
    setVarDef( varDef);
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public VarBinding( VarBindingDef def)
    {
    this( def.getVarDef(), def.getValueDef());
    }

  /**
   * Changes the variable name for this binding.
   */
  public void setVar( String varName)
    {
    assertPath( varName);
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

  /**
   * Changes the variable definition for this binding, if any.
   */
  private void setVarDef( IVarDef varDef)
    {
    varDef_ = varDef;
    }

  /**
   * Returns the variable definition for this binding, if any.
   */
  private IVarDef getVarDef()
    {
    return varDef_;
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
      builder.getStringBuffer()
        .append( ",valid=")
        .append( isValueValid());
      }

    return builder.toString();
    }

  public int compareTo( VarBinding other)
    {
    IVarDef varDef = getVarDef();
    IVarDef otherVarDef = other.getVarDef();
    return
      varDef != null && otherVarDef != null?
      varDef.getPosition().compareTo( otherVarDef.getPosition()) :

      varDef != null?
      1 :

      otherVarDef != null?
      -1 :
      
      getVar().compareTo( other.getVar());
    }

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( getVar())
      ^ ObjectUtils.hashCode( getType())
      ^ ObjectUtils.hashCode( getValue())
      ^ (isValueValid()? Boolean.TRUE.hashCode() : Boolean.FALSE.hashCode());
    }

  @SuppressWarnings("deprecation")
  public boolean equals( Object object)
    {
    VarBinding other =
      object != null && object.getClass().equals( getClass())
      ? (VarBinding) object
      : null;

    return
      other != null
      && ObjectUtils.equals( getVar(), other.getVar())
      && ObjectUtils.equals( getType(), other.getType())
      && ObjectUtils.equals( getValue(), other.getValue())
      && isValueValid() == other.isValueValid();
    }

  private String var_;
  private String varType_;
  private String value_;
  private boolean valueValid_;
  private IVarDef varDef_;
  }
