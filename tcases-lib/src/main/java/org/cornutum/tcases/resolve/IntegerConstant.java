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
 * Defines a singleton Integer value set.
 */
public class IntegerConstant extends ConstantDomain<Integer>
  {
  /**
   * Creates a new IntegerConstant instance.
   */
  public IntegerConstant( Integer value)
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
      "int64".equals( format)
      ? getFormat()
      : format);
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  @Override
  public String getFormat()
    {
    return Optional.ofNullable( super.getFormat()).orElse( "int32");
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<Integer> dataValueOf( Integer value)
    {
    return new IntegerValue( value);
    }
  }
