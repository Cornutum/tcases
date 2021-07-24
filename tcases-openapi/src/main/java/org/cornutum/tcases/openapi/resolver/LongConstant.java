//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

/**
 * Defines a singleton Long value set.
 */
public class LongConstant extends ConstantDomain<Long>
  {
  /**
   * Creates a new LongConstant instance.
   */
  public LongConstant( Long value)
    {
    super( Type.INTEGER, value);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<Long> dataValueOf( Long value)
    {
    return new LongValue( value);
    }
  }
