//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

/**
 * Represents a generated integer value for a test case.
 */
public class IntegerValue extends DataValue<Integer>
  {
  /**
   * Creates a new IntegerValue instance.
   */
  public IntegerValue( Integer value)
    {
    this( value, "int32");
    }
  
  /**
   * Creates a new IntegerValue instance.
   */
  public IntegerValue( Integer value, String format)
    {
    super( value, Type.INTEGER, format);
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
