//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.resolve.UuidValue;

/**
 * Defines an enumerated UUID string value set.
 */
public class UuidEnum extends StringEnum
  {
  /**
   * Creates a new UuidEnum instance.
   */
  public UuidEnum( Iterable<String> enums)
    {
    super( assertUuids( enums), "uuid");
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
   * Reports a failure if any of the given values is not a valid uuid string. Otherwise, return the given values.
   */
  public static Iterable<String> assertUuids( Iterable<String> values)
    {
    toStream( values)
      .filter( value -> !UuidConstant.isUuid( value))
      .findFirst()
      .ifPresent( value -> {
        throw new ValueDomainException( String.format( "Value=%s is not a valid UUID", value));
        });

    return values;
    }
  }
