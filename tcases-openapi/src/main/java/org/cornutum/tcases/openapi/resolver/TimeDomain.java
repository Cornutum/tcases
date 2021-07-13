//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import org.cornutum.tcases.util.ToString;

import static org.apache.commons.lang3.StringUtils.rightPad;

import java.util.Calendar;
import java.util.Date;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.Calendar.*;

/**
 * Base class for a set of string-encoded time values that can be used by a request.
 */
public abstract class TimeDomain extends AbstractStringDomain
  {  
  /**
   * Creates a new TimeDomain instance.
   */
  protected TimeDomain( int maxLength, Date minDate, Date maxDate)
    {
    super( maxLength, Characters.ASCII);
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
    minDate.set( MONTH, 0);
    minDate.set( DAY_OF_MONTH, 1);
    minDate.set( HOUR_OF_DAY, 0);
    minDate.set( MINUTE, 0);
    minDate.set( SECOND, 0);
    minDate.set( MILLISECOND, 0);
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
    maxDate.set( YEAR, maxDate.get( YEAR) + 2);
    maxDate.set( MONTH, 0);
    maxDate.set( DAY_OF_MONTH, 1);
    maxDate.set( HOUR_OF_DAY, 0);
    maxDate.set( MINUTE, 0);
    maxDate.set( SECOND, 0);
    maxDate.set( MILLISECOND, 0);
    return maxDate.getTime();
    }

  /**
   * Changes the values excluded from this domain.
   */
  @Override
public void setExcluded( Set<String> excluded)
    {
    super.setExcluded( excluded);
    getExcluded().stream().forEach( value -> assertValidTime( value));
    }

  /**
   * Returns a random sequence of possible members of this domain.
   */
  @Override
protected Stream<String> newValues( ResolverContext context)
    {
    return timeValues( context).map( this::format).map( value -> withLength( context, value));
    }

  /**
   * Returns the given time value string adjusted to the current length range.
   */
  private String withLength( ResolverContext context, String value)
    {
    int length = getLengthRange().selectValue( context);

    // If invalid length, ensure time value is invalid.
    return
      length < value.length()?
      value.substring( 0, length) :

      length > value.length()?
      rightPad( value, length, '0')
      
      : value;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  @Override
public boolean contains( String value)
    {
    return
      super.contains( value)
      && isValidTime( value);
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  @Override
protected String newValue( ResolverContext context, int length)
    {
    throw new UnsupportedOperationException();
    }
  
  /**
   * Returns a random sequence of time values from this domain.
   */
  protected abstract Stream<Date> timeValues( ResolverContext context);

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

  @Override
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
