//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.Arrays;

/**
 * Defines a singleton byte array value set.
 */
public class BinaryConstant extends ConstantDomain<byte[]>
  {
  /**
   * Creates a new BinaryConstant instance.
   */
  public BinaryConstant( byte[] value)
    {
    super( Type.STRING, value);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  @Override
  public boolean contains( byte[] value)
    {
    return Arrays.equals( value, getValue());
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
    return "binary";
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<byte[]> dataValueOf( byte[] value)
    {
    return new BinaryValue( value);
    }
  }
