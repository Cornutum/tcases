//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

/**
 * Defines a singleton Boolean value set.
 */
public class BooleanConstant extends ConstantDomain<Boolean>
  {
  /**
   * Creates a new BooleanConstant instance.
   */
  public BooleanConstant( Boolean value)
    {
    super( Type.BOOLEAN, value);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Boolean> dataValueOf( Boolean value)
    {
    return new BooleanValue( value);
    }
  }
