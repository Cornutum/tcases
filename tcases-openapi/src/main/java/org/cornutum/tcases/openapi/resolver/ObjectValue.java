//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.util.Map;

/**
 * Represents a generated object value for a request case.
 */
public class ObjectValue extends DataValue<Map<String,DataValue<?>>>
  {
  /**
   * Creates a new ObjectValue instance.
   */
  public ObjectValue( Map<String,DataValue<?>> value)
    {
    super( value, Type.OBJECT, null);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
