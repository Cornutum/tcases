//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.AllOf;
import org.cornutum.tcases.conditions.ICondition;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.stream.Stream;

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
  @Override
public ICondition getCondition()
    {
    return condition_;
    }

  /**
   * Returns the effective condition that defines when this element is applicable,
   * given the specified context condition.
   */
  public ICondition getEffectiveCondition( ICondition contextCondition)
    {
    ICondition condition = getCondition();
    ICondition effCondition;

    if( condition == null && contextCondition == null)
      {
      effCondition = ICondition.ALWAYS;
      }

    else if( condition == null && contextCondition != null)
      {
      // Nothing to add to context
      effCondition = contextCondition;
      }

    else if( condition != null && (contextCondition == null || contextCondition.equals( condition)))
      {
      // Nothing to add to current condition
      effCondition = condition;
      }

    else
      {
      // Create new conjunction with context condition.
      AllOf conjunction = new AllOf();
      Stream.of( condition, contextCondition)
        .flatMap( c -> c instanceof AllOf? toStream( ((AllOf) c).getConditions()) : Stream.of(c))
        .forEach( c -> conjunction.add( c));

      effCondition = conjunction;
      }

    return effCondition;
    }
  
  private ICondition condition_;
  }

