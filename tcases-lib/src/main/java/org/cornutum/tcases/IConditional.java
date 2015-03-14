//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;

/**
 * A conditional element.
 *
 */
public interface IConditional
  {
  /**
   * Returns the condition that defines when this element is applicable.
   */
  public ICondition getCondition();
  }

