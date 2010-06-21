//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;

import com.startingblocktech.tcases.ICondition;
import com.startingblocktech.tcases.PropertySet;
import com.startingblocktech.tcases.util.ToString;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Defines a set of {@link ICondition conditions}.
 *
 * @version $Revision$, $Date$
 */
public abstract class ConditionSet implements ICondition
  {
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public abstract boolean satisfied( PropertySet properties);

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
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
  public Collection<ICondition> getConditions()
    {
    return conditions_;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( conditions_)
      .toString();
    }

  private List<ICondition> conditions_ = new ArrayList<ICondition>();
  }

