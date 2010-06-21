//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;

import com.startingblocktech.tcases.ICondition;
import com.startingblocktech.tcases.PropertySet;

import java.util.Iterator;

/**
 * A {@link ICondition condition} that is satisfied if and only if all members of a given set
 * of conditions are satisfied. Defines a logical "and" condition.
 *
 * @version $Revision$, $Date$
 */
public class AllOf extends ConditionSet
  {
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public boolean satisfied( PropertySet properties)
    {
    boolean isSatisfied;
    Iterator<ICondition> conditions;
    
    for( conditions = getConditions().iterator(),
           isSatisfied = true;
           

         isSatisfied
           && conditions.hasNext();

         isSatisfied = conditions.next().satisfied( properties));
    
    return isSatisfied;
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  public boolean compatible( PropertySet properties)
    {
    boolean isCompatible;
    Iterator<ICondition> conditions;
    
    for( conditions = getConditions().iterator(),
           isCompatible = true;
           

         isCompatible
           && conditions.hasNext();

         isCompatible = conditions.next().compatible( properties));
    
    return isCompatible;
    }
  }

