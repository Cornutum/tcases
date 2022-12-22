//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.Optional;

/**
 * Defines a singleton Long value set.
 */
public class LongConstant extends ConstantDomain<Long>
  {
  /**
   * Creates a new LongConstant instance.
   */
  public LongConstant( Long value)
    {
    super( Type.INTEGER, value);
    }

  /**
   * Changes the format for values that belong to this domain.
   */
  @Override
  public void setFormat( String format)
    {
    super.setFormat(
      "int32".equals( format)
      ? getFormat()
      : format);
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  @Override
  public String getFormat()
    {
    return Optional.ofNullable( super.getFormat()).orElse( "int64");
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<Long> dataValueOf( Long value)
    {
    return new LongValue( value);
    }
  }
