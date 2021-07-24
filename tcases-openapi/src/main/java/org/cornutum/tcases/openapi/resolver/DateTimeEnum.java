//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

/**
 * Defines an enumerated date-time string value set.
 */
public class DateTimeEnum extends StringEnum
  {
  /**
   * Creates a new DateTimeEnum instance.
   */
  public DateTimeEnum( Iterable<String> enums)
    {
    super( assertDateTimes( enums), "date-time");
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<String> dataValueOf( String value)
    {
    return new DateTimeValue( value);
    }

  /**
   * Reports a failure if any of the given values is not a valid dateTime string. Otherwise, return the given values.
   */
  public static Iterable<String> assertDateTimes( Iterable<String> values)
    {
    toStream( values)
      .filter( value -> !DateTimeConstant.isDateTime( value))
      .findFirst()
      .ifPresent( value -> {
        throw new ValueDomainException( String.format( "Value=%s is not a valid date-time", value));
        });

    return values;
    }
  }
