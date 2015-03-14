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
 * Thrown when the conditions required for an input variable are inconsistent
 * with the properties of a test case.
 *
 */
public class VarNotApplicableException extends BindingException
  {
  /**
   * Creates a new VarNotApplicableException object.
   */
  public VarNotApplicableException( VarBindingDef binding, PropertySet properties)
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
      .append( " is not applicable for properties=")
      .append( getProperties())
      .toString();
    }

  private PropertySet properties_;

  private static final long serialVersionUID = 8464312480138258497L;
  }
