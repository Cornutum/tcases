//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Map;

/**
 * Defines a singleton Object value set.
 */
public class ObjectConstant extends ConstantDomain<Map<String,DataValue<?>>>
  {
  /**
   * Creates a new ObjectConstant instance.
   */
  public ObjectConstant( ObjectValue object)
    {
    super( Type.OBJECT, object.getValue());
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Map<String,DataValue<?>>> dataValueOf( Map<String,DataValue<?>> value)
    {
    return new ObjectValue( value);
    }
  }
