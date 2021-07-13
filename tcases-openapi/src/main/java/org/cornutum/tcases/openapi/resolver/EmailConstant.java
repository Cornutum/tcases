//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;

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
    this( value, Characters.ASCII);
    }
  
  /**
   * Creates a new EmailConstant instance.
   */
  public EmailConstant( String value, Characters chars)
    {
    super( assertEmail( value), chars);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
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
      throw new ValueDomainException( String.format( "Value=%s is not a valid email", value));
      }

    return value;
    }
  }
