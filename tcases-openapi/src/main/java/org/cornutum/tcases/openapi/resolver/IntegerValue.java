//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Represents a generated integer value for a request case.
 */
public class IntegerValue extends DataValue<Integer>
  {
  /**
   * Creates a new IntegerValue instance.
   */
  public IntegerValue( Integer value)
    {
    super( value, Type.INTEGER, "int32");
    }
  }
