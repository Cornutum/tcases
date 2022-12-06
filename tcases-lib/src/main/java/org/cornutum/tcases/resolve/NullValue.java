//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated null value for a request case.
 */
public class NullValue extends DataValue<Object>
  {
  /**
   * Creates a new NullValue instance.
   */
  public NullValue()
    {
    this( null);
    }
  
  /**
   * Creates a new NullValue instance.
   */
  public NullValue( String format)
    {
    super( null, Type.NULL, format);
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
