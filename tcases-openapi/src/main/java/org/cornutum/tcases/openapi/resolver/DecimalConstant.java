//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.math.BigDecimal;

/**
 * Defines a singleton Decimal value set.
 */
public class DecimalConstant extends ConstantDomain<BigDecimal>
  {
  /**
   * Creates a new DecimalConstant instance.
   */
  public DecimalConstant( BigDecimal value)
    {
    super( Type.NUMBER, value);
    }
  }
