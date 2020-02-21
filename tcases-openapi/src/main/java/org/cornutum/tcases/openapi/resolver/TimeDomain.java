//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.util.Calendar;
import java.util.Date;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.Calendar.*;

/**
 * Base class for a set of string-encoded time values that can be used by a request.
 */
public abstract class TimeDomain extends RestrictedStringDomain
  {  
  /**
   * Creates a new TimeDomain instance.
   */
  protected TimeDomain( int maxLength, Date minDate, Date maxDate)
    {
    super( maxLength);
    setDateRange( minDate, maxDate);
    }

  /**
   * Changes the (inclusive) date range for this domain.
   */
  protected void setDateRange( Date minDate, Date maxDate)
    {
    minDate_ = Optional.ofNullable( minDate).orElse( getDefaultMinDate());
    maxDate_ = Optional.ofNullable( maxDate).orElse( getDefaultMaxDate());
    }

  /**
   * Returns the minimum date for this domain.
   */
  public Date getMinDate()
    {
    return minDate_;
    }

  /**
   * Returns the default minimum date for this domain.
   */
  private Date getDefaultMinDate()
    {
    Calendar minDate = Calendar.getInstance();
    minDate.set( YEAR, minDate.get( YEAR) - 1);
    return minDate.getTime();
    }

  /**
   * Returns the maximum date for this domain.
   */
  public Date getMaxDate()
    {
    return maxDate_;
    }

  /**
   * Returns the default maximum date for this domain.
   */
  private Date getDefaultMaxDate()
    {
    Calendar maxDate = Calendar.getInstance();
    maxDate.set( YEAR, maxDate.get( YEAR) + 1);
    return maxDate.getTime();
    }

  /**
   * Changes the values excluded from this domain.
   */
  public void setExcluded( Set<String> excluded)
    {
    super.setExcluded( excluded);
    getExcluded().stream().forEach( value -> assertValidTime( value));
    }

  /**
   * Returns a random sequence of possible members of this domain.
   */
  protected Stream<String> candidates( Random random)
    {
    return timeValues( random).map( this::format);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return
      super.contains( value)
      && isValidTime( value);
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected String newValue( Random random, int length)
    {
    throw new UnsupportedOperationException();
    }
  
  /**
   * Returns a random sequence of time values from this domain.
   */
  protected abstract Stream<Date> timeValues( Random random);

  /**
   * Reports a failure if the given value is not a valid time string. Otherwise, returns the given value.
   */
  protected abstract String assertValidTime( String value);

  /**
   * Returns true if the given value is a valid time string.
   */
  protected abstract boolean isValidTime( String value);

  /**
   * Returns the string representation of the given time.
   */
  protected abstract String format( Date time);

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( format( getMinDate()))
      .append( format( getMaxDate()))
      .toString();
    }

  private Date minDate_;
  private Date maxDate_;
}
