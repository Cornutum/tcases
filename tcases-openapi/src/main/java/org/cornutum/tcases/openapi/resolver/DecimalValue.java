//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.math.BigDecimal;

/**
 * Represents a generated de value for a request case.
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
  public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }
  }
