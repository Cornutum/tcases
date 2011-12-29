//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

/**
 * Base class for conditional elements.
 *
 * @version $Revision$, $Date$
 */
public abstract class Conditional
  {
  /**
   * Changes the condition that defines when this element is applicable.
   */
  public void setCondition( ICondition condition)
    {
    condition_ =
      condition == null
      ? ICondition.ALWAYS
      : condition;
    }

  /**
   * Returns the condition that defines when this element is applicable.
   */
  public ICondition getCondition()
    {
    return condition_;
    }
  
  private ICondition condition_;
  }

