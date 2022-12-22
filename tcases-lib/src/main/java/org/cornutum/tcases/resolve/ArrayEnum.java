//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.Collection;
import java.util.List;

/**
 * Defines an enumerated array value set.
 */
public class ArrayEnum<T> extends EnumDomain<List<DataValue<T>>>
  {  
  /**
   * Creates a new ArrayEnum instance.
   */
  public ArrayEnum( Collection<List<DataValue<T>>> enums)
    {
    super( Type.ARRAY, enums);
    }

  /**
   * Returns the value represented by the given string.
   */
  @Override
  protected List<DataValue<T>> valueOf( String value)
    {
    throw new UnsupportedOperationException();
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected ArrayValue<T> dataValueOf( List<DataValue<T>> value)
    {
    return new ArrayValue<T>( value, getFormat());
    }
  }
