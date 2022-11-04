//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.Collection;

import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.resolve.LongValue;

/**
 * Defines an enumerated long value set.
 */
public class LongEnum extends EnumDomain<Long>
  {
  /**
   * Creates a new LongEnum instance.
   */
  public LongEnum( Iterable<String> enums)
    {
    super( Type.INTEGER, enums);
    }
  
  /**
   * Creates a new LongEnum instance.
   */
  public LongEnum( Collection<Long> enums)
    {
    super( Type.INTEGER, enums);
    }

  /**
   * Returns the value represented by the given string.
   */
  @Override
  protected Long valueOf( String value)
    {
    try
      {
      return Long.valueOf( value);
      }
    catch( Exception e)
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid long number", value), e);
      }
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
