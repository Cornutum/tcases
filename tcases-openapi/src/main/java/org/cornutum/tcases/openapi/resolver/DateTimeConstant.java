//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.FormattedString;

import java.util.Date;

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
   * Returns a {@link DataValue} for the given value in this domain.
   */
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
      toTime( value);
      return true;
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
  }
