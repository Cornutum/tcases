//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated binary value for a request case.
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
  public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
