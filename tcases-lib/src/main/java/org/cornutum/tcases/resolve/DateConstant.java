//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import java.util.regex.Pattern;

/**
 * Defines a singleton date string value set.
 */
public class DateConstant extends StringConstant
  {
  /**
   * Creates a new DateConstant instance.
   */
  public DateConstant( String value)
    {
    super( assertDate( value));
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
   * Returns true if the given value is a valid date string.
   */
  public static boolean isDate( String value)
    {
    try
      {
      return
        date_.matcher( value).matches()
        && FormattedString.getDateFormat().parse( value) != null;
      }
    catch( Exception e)
      {
      return false;
      }
    }

  /**
   * Reports a failure if the given value is not a valid date string. Otherwise, return the given value.
   */
  public static String assertDate( String value)
    {
    if( !isDate( value))
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid date", value));
      }

    return value;
    }

  private static final Pattern date_ = Pattern.compile( "[0-9]{4}-[0-9]{2}-[0-9]{2}");
  }
