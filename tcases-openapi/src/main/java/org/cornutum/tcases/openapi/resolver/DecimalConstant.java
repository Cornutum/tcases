//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

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
    this( value, null);
    }
  
  /**
   * Creates a new DecimalConstant instance.
   */
  public DecimalConstant( BigDecimal value, String format)
    {
    super( Type.NUMBER, value);
    format_ = format;
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
protected DataValue<BigDecimal> dataValueOf( BigDecimal value)
    {
    return new DecimalValue( value, format_);
    }

  private final String format_;
  }
