//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

/**
 * An {@link ExecutionContext} that uses a {@link Notifer} to report execution conditions.
 */
public abstract class ExecutionNotifier<E extends RuntimeException> extends ExecutionContext<E>
  {

  /**
   * Changes the {@link Notifier} for this context.
   */
  public void setNotifier( Notifier notifier)
    {
    notifier_ = notifier;
    }

  /**
   * Returns the {@link Notifier} for this context.
   */
  public Notifier getNotifier()
    {
    return notifier_;
    }    

  /**
   * Reports a condition that will affect the expected result.
   *
   * @param reason  A description of the condition
   */
  public void warn( String reason)
    {
    getNotifier().warn( getLocation(), reason);
    }

  /**
   * Reports an error that could produce an incorrect result.
   *
   * @param reason  A description of the problem
   * @param resolution  A description of how the problem was resolved
   */
  public void error( String reason, String resolution)
    {
    getNotifier().error( getLocation(), reason, resolution);
    }

  private Notifier notifier_;
  }
