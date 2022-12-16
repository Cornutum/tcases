//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated binary value for a test case.
 */
public class BinaryValue extends DataValue<byte[]>
  {
  /**
   * Creates a new BinaryValue instance.
   */
  public BinaryValue( byte[] value)
    {
    super( value, Type.STRING, "binary");
    }

  /**
   * Implements the Visitor pattern for this data value.
   */
  @Override
  public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
