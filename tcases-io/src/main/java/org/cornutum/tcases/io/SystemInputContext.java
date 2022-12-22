//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.util.ExecutionNotifier;
import org.cornutum.tcases.util.Notifier;

import org.slf4j.Logger;

/**
 * Defines the context for reading or writing a {@link org.cornutum.tcases.SystemInputDef}.
 */
public class SystemInputContext extends ExecutionNotifier<SystemInputException>
  {
  /**
   * Creates a new SystemInputContext instance.
   */
  public SystemInputContext()
    {
    this( Notifier.ignore());
    }
  
  /**
   * Creates a new SystemInputContext instance.
   */
  public SystemInputContext( Logger logger)
    {
    this( Notifier.log( logger));
    }
  
  /**
   * Creates a new SystemInputContext instance.
   */
  public SystemInputContext( Notifier notifier)
    {
    setNotifier( notifier);
    }
  
  /**
   * Returns an exception to throw for the given failure.
   */
  @Override
  protected SystemInputException whenFailure( Throwable e)
    {
    return
      SystemInputException.class.isAssignableFrom( e.getClass())
      ? (SystemInputException) e
      : new SystemInputException( getLocation(), e);
    }

  }
