//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import java.util.function.Supplier;

/**
 * Base class for objects that detect and report execution conditions.
 */
public abstract class ConditionReporter<C extends ExecutionContext<?>>
  {
  /**
   * Creates a new ConditionReporter instance.
   */
  protected ConditionReporter( C context)
    {
    context_ = context;
    }
  
  /**
   * Changes the condition notifier for this reporter.
   */
  protected void setNotifier( Notifier notifier)
    {
    notifier_ = notifier;
    }

  /**
   * Returns the condition notifier for this reporter.
   */
  protected Notifier getNotifier()
    {
    return notifier_;
    }

  /**
   * Returns the execution context for this reporter.
   */
  protected C getContext()
    {
    return context_;
    }

  /**
   * Reports a warning condition
   */
  protected void notifyWarning( String reason)
    {
    getNotifier().warn( getContext().getLocation(), reason);
    }

  /**
   * Reports an error condition
   */
  protected void notifyError( String reason, String resolution)
    {
    getNotifier().error( getContext().getLocation(), reason, resolution);
    }

  /**
   * Returns the result of the given supplier within the specified context.
   */
  protected <T> T resultFor( String context, Supplier<T> supplier)
    {
    return getContext().resultFor( context, supplier);
    }

  /**
   * Performs the given action within the specified context.
   */
  protected void doFor( String context, Runnable action)
    {
    getContext().doFor( context, action);
    }

  private final C context_;
  private Notifier notifier_;
  }
