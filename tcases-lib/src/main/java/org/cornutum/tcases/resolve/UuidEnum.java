//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

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
