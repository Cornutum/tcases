//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

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
    super( null, Type.NULL, null);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
