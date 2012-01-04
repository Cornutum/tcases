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
public abstract class Conditional implements IConditional
  {
  /**
   * Changes the condition that defines when this element is applicable.
   */
  public void setCondition( ICondition condition)
    {
    condition_ = condition;
    }

  /**
   * Returns the condition that defines when this element is applicable.
   */
  public ICondition getCondition()
    {
    return condition_;
    }

  /**
   * Returns the effective condition that defines when this element is applicable.
   */
  public static ICondition acquireCondition( IConditional conditional)
    {
    ICondition condition = conditional.getCondition();
    return
      condition == null
      ? ICondition.ALWAYS
      : condition;
    }
  
  private ICondition condition_;
  }

