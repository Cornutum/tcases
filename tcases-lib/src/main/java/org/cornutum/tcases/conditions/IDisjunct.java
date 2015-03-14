//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;


import java.util.Iterator;

/**
 * A {@link ICondition condition} that defined by the disjunction (logical OR) of one or
 * more {@link IAssertion assertions}.
 *
 */
public interface IDisjunct extends IConjunct
  {
  /**
   * Returns the assertions for this disjunction.
   */
  Iterator<IAssertion> getAssertions();

  /**
   * Returns the number of assertions for this disjunction.
   */
  int getAssertionCount();

  /**
   * Returns true if the given assertion is a member of this disjunction.
   */
  boolean contains( IAssertion assertion);  
  }
