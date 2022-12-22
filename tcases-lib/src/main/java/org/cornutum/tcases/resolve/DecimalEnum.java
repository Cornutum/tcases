//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.math.BigDecimal;
import java.util.Collection;

/**
 * Defines an enumerated decimal value set.
 */
public class DecimalEnum extends EnumDomain<BigDecimal>
  {
  /**
   * Creates a new DecimalEnum instance.
   */
  public DecimalEnum( Iterable<String> enums, String format)
    {
    super( Type.NUMBER, enums);
    setFormat( format);
    }
  
  /**
   * Creates a new DecimalEnum instance.
   */
  public DecimalEnum( Collection<BigDecimal> enums, String format)
    {
    super( Type.NUMBER, enums);
    setFormat( format);
    }

  /**
   * Returns the value represented by the given string.
   */
  @Override
  protected BigDecimal valueOf( String value)
    {
    try
      {
      return new BigDecimal( value);
      }
    catch( Exception e)
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid decimal number", value), e);
      }
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<BigDecimal> dataValueOf( BigDecimal value)
    {
    return new DecimalValue( value, getFormat());
    }
  }
