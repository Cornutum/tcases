//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
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
    this( enums, format, Characters.ANY);
    }
  
  /**
   * Creates a new StringEnum instance.
   */
  public StringEnum( Iterable<String> enums, String format, Characters chars)
    {
    super( Type.STRING, enums);
    format_ = format;

    for( String value : enums)
      {
      if( !chars.allowed( value))
        {
        throw new ValueDomainException( String.format( "'%s' is not allowed by %s", value, chars));
        }
      }
    }

  /**
   * Returns the value represented by the given string.
   */
  @Override
  protected String valueOf( String value)
    {
    return value;
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<String> dataValueOf( String value)
    {
    return new StringValue( value, format_);
    }

  private final String format_;
  }
