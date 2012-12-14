//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;


import java.util.Iterator;

/**
 * A {@link ICondition condition} that defined by the conjunction (logical AND) of one or
 * more {@link IDisjunct disjunctions}.
 *
 * @version $Revision$, $Date$
 */
public interface IConjunct extends ICondition
  {
  /**
   * Returns the disjunctions for this conjunction.
   */
  public Iterator<IDisjunct> getDisjuncts();

  /**
   * Returns the number of disjunctions for this conjunction.
   */
  int getDisjunctCount();
  }
