//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

/**
 * A conditional element.
 *
 * @version $Revision$, $Date$
 */
public interface IConditional
  {
  /**
   * Returns the condition that defines when this element is applicable.
   */
  public ICondition getCondition();
  }

