//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

/**
 * Represents a generated long value for a test case.
 */
public class LongValue extends DataValue<Long>
  {
  /**
   * Creates a new LongValue instance.
   */
  public LongValue( Long value)
    {
    this( value, "int64");
    }
  
  /**
   * Creates a new LongValue instance.
   */
  public LongValue( Long value, String format)
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
