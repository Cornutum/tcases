//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

/**
 * A condition that evaluates a set of test case properties.
 *
 */
public interface ICondition
  {
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  boolean satisfied( PropertySet properties);

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  boolean compatible( PropertySet properties);

  /**
   * Implements the Visitor pattern for this condition.
   */
  void accept( IConditionVisitor visitor);

  /**
   * A {@link ICondition condition} that is always satisfied by any {@link PropertySet}.
   */
  ICondition ALWAYS = new AllOf();
  }
