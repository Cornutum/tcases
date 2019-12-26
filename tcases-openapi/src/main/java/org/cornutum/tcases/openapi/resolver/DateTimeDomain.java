//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.FormattedString;
import static org.cornutum.tcases.openapi.resolver.DateTimeConstant.*;

import java.util.Date;
import java.util.Random;
import java.util.stream.Stream;

/**
 * Defines a set of date-time string values that can be used by a request.
 */
public class DateTimeDomain extends TimeDomain
  {
  /**
   * Creates a new DateTimeDomain instance.
   */
  public DateTimeDomain()
    {
    this( null, null);
    }

  /**
   * Creates a new DateTimeDomain instance.
   */
  public DateTimeDomain( Date minDate, Date maxDate)
    {
    super( 30, minDate, maxDate);
    }

  /**
   * Defines the initial length range for values in this domain.
   */
  protected void initLengthRange()
    {
    setLengthRange( new IntegerConstant( 29));
    }

  /**
   * Returns true if the given values are equal.
   */
  protected boolean valuesEqual( String value1, String value2)
    {
    return toTime( value1).equals( toTime( value2));
    }

  /**
   * Returns a random sequence of time values from this domain.
   */
  protected Stream<Date> timeValues( Random random)
    {
    long minTime = getMinDate().getTime();
    long maxTime = getMaxDate().getTime();
    long millis = maxTime - minTime + 1;

    return
      millis < 1?
      Stream.empty() :

      millis == 1?
      Stream.of( getMinDate()) :

      random.longs( 0, millis)
      .map( m -> minTime + m)
      .mapToObj( Date::new);
    }

  /**
   * Reports a failure if the given value is not a valid time string. Otherwise, returns the given value.
   */
  protected String assertValidTime( String value)
    {
    return assertDateTime( value);
    }

  /**
   * Returns true if the given value is a valid time string.
   */
  protected boolean isValidTime( String value)
    {
    return isDateTime( value);
    }

  /**
   * Returns the string representation of the given time.
   */
  protected String format( Date time)
    {
    return FormattedString.getDateTimeFormat().format( time);
    }
}
