//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import org.cornutum.tcases.util.Characters;

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
    setFormat( format);

    if( !chars.allowed( value))
      {
      throw new ValueDomainException( String.format( "'%s' is not allowed by %s", value, chars));
      }
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<String> dataValueOf( String value)
    {
    return new StringValue( value, getFormat());
    }
  }
