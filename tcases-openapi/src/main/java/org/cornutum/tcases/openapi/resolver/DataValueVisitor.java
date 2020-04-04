//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Defines the Visitor pattern for {@link DataValue} objects.
 */
public interface DataValueVisitor
  {
  public void visit( ArrayValue<?> data);

  public void visit( BinaryValue data);

  public void visit( BooleanValue data);

  public void visit( DecimalValue data);

  public void visit( IntegerValue data);

  public void visit( LongValue data);

  public void visit( NullValue data);

  public void visit( ObjectValue data);

  public void visit( StringValue data);
  }
