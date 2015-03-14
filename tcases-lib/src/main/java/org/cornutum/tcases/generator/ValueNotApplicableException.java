//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.VarDef;

/**
 * Thrown when an input variable binding makes an currently bound variable
 * inapplicable.
 *
 */
public class ValueNotApplicableException extends BindingException
  {
  /**
   * Creates a new ValueNotApplicableException object.
   */
  public ValueNotApplicableException( VarBindingDef binding, VarDef var)
    {
    super( binding);
    setVar( var);
    }

  /**
   * Changes the variable made inapplicable.
   */
  private void setVar( VarDef var)
    {
    var_ = var;
    }

  /**
   * Returns the variable made inapplicable.
   */
  public VarDef getVar()
    {
    return var_;
    }

  public String getMessage()
    {
    return
      new StringBuilder()
      .append( "Can't add binding=")
      .append( getBinding())
      .append( ": variable=")
      .append( getVar())
      .append( " would become inapplicable")
      .toString();
    }

  private VarDef var_;
  private static final long serialVersionUID = 7236058955114135486L; 
  }
