//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

/**
 * Defines a singleton String value set.
 */
public class StringConstant extends ConstantDomain<String>
  {
  /**
   * Creates a new StringConstant instance.
   */
  public StringConstant( String value)
    {
    this( value, (String) null);
    }
  
  /**
   * Creates a new StringConstant instance.
   */
  public StringConstant( String value, Characters chars)
    {
    this( value, null, chars);
    }
  
  /**
   * Creates a new StringConstant instance.
   */
  public StringConstant( String value, String format)
    {
    this( value, format, Characters.ANY);
    }
  
  /**
   * Creates a new StringConstant instance.
   */
  public StringConstant( String value, String format, Characters chars)
    {
    super( Type.STRING, value);
    format_ = format;

    if( !chars.allowed( value))
      {
      throw new ValueDomainException( String.format( "'%s' is not allowed by %s", value, chars));
      }
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
