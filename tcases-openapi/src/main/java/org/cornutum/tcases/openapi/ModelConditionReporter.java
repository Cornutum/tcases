//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import java.util.function.Supplier;

/**
 * Base class for objects that detect and report input modelling conditions.
 */
public abstract class ModelConditionReporter
  {
  /**
   * Changes the NotificationContext for this reporter.
   */
  protected void setContext( NotificationContext context)
    {
    context_ = context;
    }

  /**
   * Returns the NotificationContext for this reporter.
   */
  protected NotificationContext getContext()
    {
    return context_;
    }

  /**
   * Report an warning condition
   */
  protected void notifyWarning( String reason)
    {
    context_.warn( reason);
    }

  /**
   * Report an error condition
   */
  protected void notifyError( String reason, String resolution)
    {
    context_.error( reason, resolution);
    }

  /**
   * Returns the result of the given supplier within the specified context.
   */
  protected <T> T resultFor( String context, Supplier<T> supplier)
    {
    return context_.resultFor( context, supplier);
    }

  /**
   * Performs the given action within the specified context.
   */
  protected void doFor( String context, Runnable action)
    {
    context_.doFor( context, action);
    }

  private NotificationContext context_;
  }
