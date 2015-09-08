//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.VarValueDef;

/**
 * Thrown when binding a variable that is already bound to a different value.
 *
 */
public class VarBoundException extends BindingException
  {
  /**
   * Creates a new VarBoundException object.
   */
  public VarBoundException( VarBindingDef binding, VarValueDef value)
    {
    super( binding);
    setValue( value);
    }

  /**
   * Changes the current value of this variable.
   */
  private void setValue( VarValueDef value)
    {
    value_ = value;
    }

  /**
   * Returns the current value of this variable.
   */
  public VarValueDef getValue()
    {
    return value_;
    }

  public String getMessage()
    {
    return
      new StringBuilder()
      .append( "Can't add binding=")
      .append( getBinding())
      .append( ": variable already bound to value=")
      .append( getValue().getName())
      .toString();
    }

  private VarValueDef value_;
  private static final long serialVersionUID = 3458618387667887028L;
  }
