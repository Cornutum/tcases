//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.util.ToString;

/**
 * A {@link ICondition condition} that is satisfied if and only number of instances of a property equals a specified minimum.
 */
public class Equals extends Between
  {
  /**
   * Creates a new Equals instance.
   */
  public Equals( String property, int count)
    {
    super( new AssertNotLess( property, count), new AssertNotMore( property, count));
    }

  /**
   * Implements the Visitor pattern for this condition.
   */
  @Override
  public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "property", getMin().getProperty())
      .append( "count", getMin().getBound())
      .toString();
    }

  }

