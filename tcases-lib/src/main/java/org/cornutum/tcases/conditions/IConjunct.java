//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;


import java.util.Iterator;

/**
 * A {@link ICondition condition} that defined by the conjunction (logical AND) of one or
 * more {@link IDisjunct disjunctions}.
 *
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
