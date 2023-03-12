//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

import java.util.Map;

/**
 * Represents a generated object value for a test case.
 */
public class ObjectValue extends DataValue<Map<String,DataValue<?>>>
  {
  /**
   * Creates a new ObjectValue instance.
   */
  public ObjectValue( Map<String,DataValue<?>> value)
    {
    this( value, null);
    }
  
  /**
   * Creates a new ObjectValue instance.
   */
  public ObjectValue( Map<String,DataValue<?>> value, String format)
    {
    super( value, Type.OBJECT, format);
    }

  /**
   * Implements the Visitor pattern for this data value.
   */
  @Override
  public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
