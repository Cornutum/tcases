//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Represents a generated boolean value for a request case.
 */
public class BooleanValue extends DataValue<Boolean>
  {
  /**
   * Creates a new BooleanValue instance.
   */
  public BooleanValue( Boolean value)
    {
    super( value, Type.BOOLEAN, null);
    }
  }
