//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;

/**
 * A {@link com.startingblocktech.tcases.conditions.ICondition condition} that asserts the (non)existence of a single property.
 *
 * @version $Revision$, $Date$
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
