//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

/**
 * Defines an enumerated date string value set.
 */
public class DateEnum extends StringEnum
  {
  /**
   * Creates a new DateEnum instance.
   */
  public DateEnum( Iterable<String> enums)
    {
    super( assertDates( enums), "date");
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<String> dataValueOf( String value)
    {
    return new DateValue( value);
    }

  /**
   * Reports a failure if any of the given values is not a valid date string. Otherwise, return the given values.
   */
  public static Iterable<String> assertDates( Iterable<String> values)
    {
    toStream( values)
      .filter( value -> !DateConstant.isDate( value))
      .findFirst()
      .ifPresent( value -> {
        throw new ValueDomainException( String.format( "Value=%s is not a valid date", value));
        });

    return values;
    }
  }
