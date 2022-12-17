//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import java.util.List;

/**
 * Represents a generated array value for a test case.
 */
public class ArrayValue<T> extends DataValue<List<DataValue<T>>>
  {
  /**
   * Creates a new ArrayValue instance.
   */
  public ArrayValue( List<DataValue<T>> value)
    {
    this( value, null);
    }
  
  /**
   * Creates a new ArrayValue instance.
   */
  public ArrayValue( List<DataValue<T>> value, String format)
    {
    super( value, Type.ARRAY, format);
    }

  /**
   * Implements the Visitor pattern for this data value.
   */
  @Override
  public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }
  }
