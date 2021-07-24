//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;
import org.cornutum.tcases.util.ToString;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Defines a set of {@link ICondition conditions}.
 *
 */
public abstract class ConditionSet implements ICondition
  {
  /**
   * Creates a new ConditionSet instance.
   */
  protected ConditionSet( ICondition ... conditions)
    {
    for( int i = 0; i < conditions.length; i++)
      {
      add( conditions[i]);
      }
    }

  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  @Override
  public abstract boolean satisfied( PropertySet properties);

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  @Override
  public abstract boolean compatible( PropertySet properties);
  
  /**
   * Adds a condition to this set.
   */
  public ConditionSet add( ICondition condition)
    {
    conditions_.add( condition);
    return this;
    }

  /**
   * Removes a condition from this set.
   */
  public ConditionSet remove( ICondition condition)
    {
    conditions_.remove( condition);
    return this;
    }

  /**
   * Returns the conditions in this set.
   */
  public Iterator<ICondition> getConditions()
    {
    return conditions_.iterator();
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( conditions_);
    }

  @Override
  public boolean equals( Object object)
    {
    ConditionSet other =
      object != null && object.getClass().equals( getClass())
      ? (ConditionSet) object
      : null;

    return
      other != null
      && Objects.equals( other.conditions_, conditions_);
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( conditions_)
      .toString();
    }

  protected Set<ICondition> conditions_ = new LinkedHashSet<ICondition>();
  }

