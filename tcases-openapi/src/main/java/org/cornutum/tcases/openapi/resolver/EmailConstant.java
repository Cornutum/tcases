//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Defines a singleton email string value set.
 */
public class EmailConstant extends StringConstant
  {
  /**
   * Creates a new EmailConstant instance.
   */
  public EmailConstant( String value)
    {
    super( assertEmail( value));
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new EmailValue( value);
    }

  /**
   * Reports a failure if the given value is not a valid email string. Otherwise, return the given value.
   */
  public static String assertEmail( String value)
    {
    if( !EmailDomain.isEmail( value))
      {
      throw new RequestCaseException( String.format( "Value=%s is not a valid email", value));
      }

    return value;
    }
  }
