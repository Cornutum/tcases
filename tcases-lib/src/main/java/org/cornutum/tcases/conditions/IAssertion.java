//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

/**
 * A {@link org.cornutum.tcases.conditions.ICondition condition} that asserts the (non)existence of a single property.
 *
 */
public interface IAssertion extends IDisjunct
  {
  /**
   * Returns the property for this assertion.
   */
  String getProperty();

  /**
   * Returns an assertion that negates this assertion.
   */
  IAssertion negate();
  
  /**
   * Returns true if this assertion negates the other.
   */
  boolean negates( IAssertion other);
  }
