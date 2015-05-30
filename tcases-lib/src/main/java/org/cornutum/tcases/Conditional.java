//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;

/**
 * Base class for conditional elements.
 *
 */
public abstract class Conditional extends Annotated implements IConditional
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

