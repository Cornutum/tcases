//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.Characters;

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
    return "email";
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
