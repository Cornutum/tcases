//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Defines a singleton String value set.
 */
public class DateTimeConstant extends StringConstant
  {
  /**
   * Creates a new DateTimeConstant instance.
   */
  public DateTimeConstant( String value)
    {
    super( assertDateTime( value)); 
    }

  /**
   * Changes the format for values that belong to this domain.
   */
  @Override
  public void setFormat( String format)
    {
    // This domain is defined by a specific format
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  @Override
  public String getFormat()
    {
    return "date-time";
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
   * Returns the time represented by the given value;
   */
  public static Date toTime( String value)
    {
    try
      {
      return FormattedString.getDateTimeFormat().parse( value);
      }
    catch( Exception e)
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid date-time", value));
      }
    }

  /**
   * Returns true if the given value is a valid date-time string.
   */
  public static boolean isDateTime( String value)
    {
    try
      {
      Matcher dtm = dateTime_.matcher( value);
      return
        dtm.matches()
        && toTime( formatDateTime( dtm.group(1), dtm.group(2), dtm.group(3), dtm.group(4))) != null;
      }
    catch( Exception e)
      {
      return false;
      }
    }

  /**
   * Report a failure if the given value is not a valid date-time. Otherwise, returns the value.
   */
  public static String assertDateTime( String value)
    {
    if( !isDateTime( value))
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid date-time", value));
      }

    return value;
    }

  private static String formatDateTime( String date, String time, String ms, String zone)
    {
    return
      String.format(
        "%sT%s%s%s",
        date,
        time,
        ms == null? ".000" : ms,
        zone.equals( "Z")? "+00:00" : zone);
    }
  
  private static final String date_ = "([0-9]{4}-[0-9]{2}-[0-9]{2})";
  private static final String time_ = "([0-9]{2}:[0-9]{2}:[0-9]{2})";
  private static final String ms_ = "(\\.[0-9]{3})?";
  private static final String zone_ = "(Z|[+\\-][0-9]{2}:[0-9]{2})";
  private static final Pattern dateTime_ = Pattern.compile( String.format( "%sT%s%s%s", date_, time_, ms_, zone_));
  }
