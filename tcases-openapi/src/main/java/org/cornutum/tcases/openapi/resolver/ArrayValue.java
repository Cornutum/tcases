//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.List;

/**
 * Represents a generated array value for a request case.
 */
public class ArrayValue<T> extends DataValue<List<DataValue<T>>>
  {
  /**
   * Creates a new ArrayValue instance.
   */
  public ArrayValue( List<DataValue<T>> value)
    {
    super( value, Type.ARRAY, null);
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
