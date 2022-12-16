//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import java.math.BigDecimal;

/**
 * Represents a generated decimal value for a test case.
 */
public class DecimalValue extends DataValue<BigDecimal>
  {
  /**
   * Creates a new DecimalValue instance.
   */
  public DecimalValue( BigDecimal value, String format)
    {
    super( value, Type.NUMBER, format);
    }
  
  /**
   * Creates a new DecimalValue instance.
   */
  public DecimalValue( BigDecimal value)
    {
    this( value, null);
    }

  /**
   * Implements the Visitor pattern for this data value.
   */
  @Override
  public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }
  }
