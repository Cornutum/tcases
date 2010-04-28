//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;

import com.startingblocktech.tcases.ICondition;
import com.startingblocktech.tcases.PropertySet;

import java.util.Iterator;

/**
 * A {@link ICondition condition} that is satisfied if and only if all members of a given set
 * of conditions are <EM>not</EM> satisfied. Defines a logical negation condition.
 *
 * @version $Revision$, $Date$
 */
public class Not extends ConditionSet
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

         isSatisfied = !conditions.next().satisfied( properties));
    
    return isSatisfied;
    }
  }

