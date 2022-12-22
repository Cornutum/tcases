//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import java.util.regex.Pattern;

/**
 * Defines a singleton UUID string value set.
 */
public class UuidConstant extends StringConstant
  {
  /**
   * Creates a new UuidConstant instance.
   */
  public UuidConstant( String value)
    {
    super( assertUuid( value));
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
    return "uuid";
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<String> dataValueOf( String value)
    {
    return new UuidValue( value);
    }

  /**
   * Returns true if the given value is a valid UUID string.
   */
  public static boolean isUuid( String value)
    {
    return uuid_.matcher( value).matches();
    }

  /**
   * Reports a failure if the given value is not a valid UUID string. Otherwise, return the given value.
   */
  public static String assertUuid( String value)
    {
    if( !isUuid( value))
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid UUID", value));
      }

    return value;
    }

  private static final Pattern uuid_ = Pattern.compile( "\\p{XDigit}{8}-\\p{XDigit}{4}-\\p{XDigit}{4}-\\p{XDigit}{4}-\\p{XDigit}{12}");
  }
