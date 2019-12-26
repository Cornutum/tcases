//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.FormattedString;
import static org.cornutum.tcases.openapi.resolver.DateConstant.*;

import java.util.Calendar;
import java.util.Date;
import java.util.Random;
import java.util.stream.Stream;
import static java.util.Calendar.*;

/**
 * Defines a set of date string values that can be used by a request.
 */
public class DateDomain extends TimeDomain
  {
  /**
   * Creates a new DateDomain instance.
   */
  public DateDomain()
    {
    this( null, null);
    }

  /**
   * Creates a new DateDomain instance.
   */
  public DateDomain( Date minDate, Date maxDate)
    {
    super( 11, minDate, maxDate);
    }

  /**
   * Defines the initial length range for values in this domain.
   */
  protected void initLengthRange()
    {
    setLengthRange( new IntegerConstant( 10));
    }

  /**
   * Returns a random sequence of time values from this domain.
   */
  protected Stream<Date> timeValues( Random random)
    {
    Calendar minDate =
      new Calendar.Builder()
      .setInstant( getMinDate())
      .build();

    long minTime =
      new Calendar.Builder()
      .setDate( minDate.get( YEAR), minDate.get( MONTH), minDate.get( DAY_OF_MONTH))
      .setTimeOfDay( 0, 0, 0, 0)
      .build()
      .getTimeInMillis();

    Calendar maxDate =
      new Calendar.Builder()
      .setInstant( getMaxDate())
      .build();

    long maxTime =
      new Calendar.Builder()
      .setDate( maxDate.get( YEAR), maxDate.get( MONTH), maxDate.get( DAY_OF_MONTH))
      .setTimeOfDay( 0, 0, 0, 0)
      .build()
      .getTimeInMillis();

    long millisPerDay = 86400000;
    long daysCount = (maxTime - minTime) / millisPerDay + 1;

    return
      daysCount < 1?
      Stream.empty() :

      daysCount == 1?
      Stream.of( getMinDate()) :

      random.longs( 0, daysCount)
      .map( d -> minTime + d * millisPerDay)
      .mapToObj( Date::new);
    }

  /**
   * Reports a failure if the given value is not a valid time string. Otherwise, returns the given value.
   */
  protected String assertValidTime( String value)
    {
    return assertDate( value);
    }

  /**
   * Returns true if the given value is a valid time string.
   */
  protected boolean isValidTime( String value)
    {
    return isDate( value);
    }

  /**
   * Returns the string representation of the given time.
   */
  protected String format( Date time)
    {
    return FormattedString.getDateFormat().format( time);
    }
}
