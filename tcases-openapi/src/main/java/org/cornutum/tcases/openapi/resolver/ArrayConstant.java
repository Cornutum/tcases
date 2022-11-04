//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.List;

import org.cornutum.tcases.resolve.ArrayValue;
import org.cornutum.tcases.resolve.DataValue;

/**
 * Defines a singleton Array value set.
 */
public class ArrayConstant extends ConstantDomain<List<DataValue<Object>>>
  {
  /**
   * Creates a new ArrayConstant instance.
   */
  public ArrayConstant( ArrayValue<Object> array)
    {
    super( Type.ARRAY, array.getValue());
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
    protected DataValue<List<DataValue<Object>>> dataValueOf( List<DataValue<Object>> value)
    {
    return new ArrayValue<Object>( value);
    }
  }
