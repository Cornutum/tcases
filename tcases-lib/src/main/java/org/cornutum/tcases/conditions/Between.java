//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import java.util.Objects;

import org.cornutum.tcases.util.ToString;

/**
 * A {@link ICondition condition} that is satisfied if and only both its minimum and maximum
 * conditions are satisified.
 */
public class Between extends AllOf
  {
  /**
   * Creates a new Between instance.
   */
  public Between( BoundedAssertion min, BoundedAssertion max)
    {
    super();

    if( !Objects.equals( min.getProperty(), max.getProperty()))
      {
      throw new IllegalArgumentException( String.format( "min property=%s not the same as max property=%s", min.getProperty(), max.getProperty()));
      }

    if( !min.completable())
      {
      throw new IllegalArgumentException( String.format( "min=%s is not a lower bound", min));
      }

    if( max.completable())
      {
      throw new IllegalArgumentException( String.format( "max=%s is not an upper bound", max));
      }

    conditions_.add( min);
    conditions_.add( max);
    }

  /**
   * Returns the minimum condition.
   */
  public BoundedAssertion getMin()
    {
    return (BoundedAssertion) conditions_.get(0);
    }

  /**
   * Returns the maximum condition.
   */
  public BoundedAssertion getMax()
    {
    return (BoundedAssertion) conditions_.get(1);
    }
  
  /**
   * Adds a condition to this set.
   */
  public ConditionSet add( ICondition condition)
    {
    throw new UnsupportedOperationException();
    }

  /**
   * Removes a condition from this set.
   */
  public ConditionSet remove( ICondition condition)
    {
    throw new UnsupportedOperationException();
    }

  /**
   * Implements the Visitor pattern for this condition.
   */
  public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "min", getMin())
      .append( "max", getMax())
      .toString();
    }
  }

