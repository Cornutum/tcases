//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

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
    this( value, null);
    }
  
  /**
   * Creates a new StringConstant instance.
   */
  public StringConstant( String value, String format)
    {
    super( Type.STRING, value);
    format_ = format;
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
