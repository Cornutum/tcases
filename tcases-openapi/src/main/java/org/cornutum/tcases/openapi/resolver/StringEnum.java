//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

/**
 * Defines an enumerated string value set.
 */
public class StringEnum extends EnumDomain<String>
  {
  /**
   * Creates a new StringEnum instance.
   */
  public StringEnum( Iterable<String> enums, String format)
    {
    super( Type.STRING, enums);
    format_ = format;
    }

  /**
   * Returns the value represented by the given string.
   */
  protected String valueOf( String value)
    {
    return value;
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new StringValue( value, format_);
    }

  private final String format_;
  }
