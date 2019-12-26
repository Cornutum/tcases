//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
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
public abstract class TimeDomain extends StringDomain
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
   * Defines a constant length range for values in this domain.
   */
  public void setLengthRange( Integer length)
    {
    if( !getLengthRange().contains( length))
      {
      throw new RequestCaseException( String.format( "Invalid value length=%s -- length must be %s", length, getLengthRange()));
      }
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Integer min, Integer max)
    {
    throw new RequestCaseException( String.format( "Invalid value length range=[%s,%s] -- length must be %s", min, max, getLengthRange()));
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Range range)
    {
    throw new RequestCaseException( String.format( "Invalid value length range=%s -- length must be %s", range, getLengthRange()));
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
   * Returns a random sequence of values from this domain.
   */
  public Stream<String> values( Random random)
    {
    return
      timeValues( random)
      .map( this::format)
      .filter( value -> isNotExcluded( value, getExcluded()));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return isValidTime( value) && super.contains( value);
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
