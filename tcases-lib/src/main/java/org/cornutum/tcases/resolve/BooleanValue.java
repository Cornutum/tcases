//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

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
    this( value, null);
    }
  
  /**
   * Creates a new BooleanValue instance.
   */
  public BooleanValue( Boolean value, String format)
    {
    super( value, Type.BOOLEAN, format);
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
