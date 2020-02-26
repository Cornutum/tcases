//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

/**
 * Defines a singleton Integer value set.
 */
public class IntegerConstant extends ConstantDomain<Integer>
  {
  /**
   * Creates a new IntegerConstant instance.
   */
  public IntegerConstant( Integer value)
    {
    super( Type.INTEGER, value);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Integer> dataValueOf( Integer value)
    {
    return new IntegerValue( value);
    }
  }
