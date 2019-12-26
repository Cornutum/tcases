//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.FormattedString;

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
   * Returns true if the given value is a valid date string.
   */
  public static boolean isDate( String value)
    {
    try
      {
      FormattedString.getDateFormat().parse( value);
      return true;
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
      throw new RequestCaseException( String.format( "Value=%s is not a valid date", value));
      }

    return value;
    }
  }
