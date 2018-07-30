//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.DefUtils.*;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Objects;

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
  public VarBinding( String varName, Object valueName)
    {
    this( varName, IVarDef.ARG, valueName);
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public VarBinding( String varName, String varType, Object valueName)
    {
    setVar( varName);
    setType( varType);
    setValue( valueName);
    setValueValid( true);
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public static VarBinding create( IVarDef varDef, VarValueDef valueDef)
    {
    VarBinding binding = 
      valueDef.isNA()
      ? new VarNaBinding( varDef.getPathName(), varDef.getType())
      : new VarBinding( varDef.getPathName(), varDef.getType(), valueDef.getName());

    binding.setValueValid( valueDef.isValid());
    binding.setVarDef( varDef);

    return binding;
    }
  
  /**
   * Creates a new VarBinding object.
   */
  public static VarBinding create( VarBindingDef def)
    {
    return create( def.getVarDef(), def.getValueDef());
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
  public void setValue( Object valueName)
    {
    assertVarValue( valueName);
    value_ = valueName;
    }

  /**
   * Returns the value name for this binding.
   */
  public Object getValue()
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
   * Returns true if this binding indicates a "not applicable" condition for this variable.
   */
  public boolean isValueNA()
    {
    return false;
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

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getVar());
    }

  public boolean equals( Object object)
    {
    VarBinding other =
      object != null && object.getClass().equals( getClass())
      ? (VarBinding) object
      : null;

    return
      other != null
      && Objects.equals( getVar(), other.getVar());
    }

  private String var_;
  private String varType_;
  private Object value_;
  private boolean valueValid_;
  private IVarDef varDef_;
  }
