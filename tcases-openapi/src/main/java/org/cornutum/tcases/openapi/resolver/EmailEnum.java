//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

/**
 * Defines an enumerated email string value set.
 */
public class EmailEnum extends StringEnum
  {
  /**
   * Creates a new EmailEnum instance.
   */
  public EmailEnum( Iterable<String> enums)
    {
    this( enums, Characters.ASCII);
    }
  
  /**
   * Creates a new EmailEnum instance.
   */
  public EmailEnum( Iterable<String> enums, Characters chars)
    {
    super( assertEmails( enums), "email", chars);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new EmailValue( value);
    }

  /**
   * Reports a failure if any of the given values is not a valid email string. Otherwise, return the given values.
   */
  public static Iterable<String> assertEmails( Iterable<String> values)
    {
    toStream( values)
      .filter( value -> !EmailDomain.isEmail( value))
      .findFirst()
      .ifPresent( value -> {
        throw new ValueDomainException( String.format( "Value=%s is not a valid email", value));
        });

    return values;
    }
  }
