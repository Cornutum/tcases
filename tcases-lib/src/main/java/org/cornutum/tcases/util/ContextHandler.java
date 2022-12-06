//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.function.Supplier;

/**
 * Base class for objects that use an {@link ExecutionNotifier} to handle execution conditions.
 */
public abstract class ContextHandler<C extends ExecutionNotifier<?>>
  {
  /**
   * Creates a new ContextHandler instance.
   */
  protected ContextHandler( C context)
    {
    context_ = context;
    }
  
  /**
   * Changes the condition notifier for this reporter.
   */
  protected void setNotifier( Notifier notifier)
    {
    getContext().setNotifier( notifier);
    }

  /**
   * Returns the condition notifier for this reporter.
   */
  protected Notifier getNotifier()
    {
    return getContext().getNotifier();
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
    getContext().warn( reason);
    }

  /**
   * Reports an error condition
   */
  protected void notifyError( String reason, String resolution)
    {
    getContext().error( reason, resolution);
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
  }