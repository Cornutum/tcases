//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.PropertySet;

/**
 * Thrown when the conditions required for an variable binding are inconsistent
 * with the properties of a test case.
 *
 */
public class ValueInconsistentException extends BindingException
  {
  /**
   * Creates a new ValueInconsistentException object.
   */
  public ValueInconsistentException( VarBindingDef binding, PropertySet properties)
    {
    super( binding);
    setProperties( properties);
    }

  /**
   * Changes the current test case properties.
   */
  private void setProperties( PropertySet properties)
    {
    properties_ = properties;
    }

  /**
   * Returns the current test case properties.
   */
  public PropertySet getProperties()
    {
    return properties_;
    }

  public String getMessage()
    {
    return
      new StringBuilder()
      .append( "Variable=")
      .append( getBinding().getVarDef())
      .append( ", value=")
      .append( getBinding().getValueDef())
      .append( " is not consistent with properties=")
      .append( getProperties())
      .toString();
    }

  private PropertySet properties_;
  private static final long serialVersionUID = -3867921539542107021L;
  }
