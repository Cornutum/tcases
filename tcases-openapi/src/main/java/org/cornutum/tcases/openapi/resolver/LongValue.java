//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Represents a generated long value for a request case.
 */
public class LongValue extends DataValue<Long>
  {
  /**
   * Creates a new LongValue instance.
   */
  public LongValue( Long value)
    {
    super( value, Type.INTEGER, "int64");
    }
  }
