//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;

/**
 * Reports conditions found during execution
 */
public interface Notifier
  {
  /**
   * Reports a condition that will affect processing results
   *
   * @param location The path to the location of the condition
   * @param reason  A description of the condition
   */
  void warn( String[] location, String reason);

  /**
   * Reports an error that would have resulted in inconsistent or infeasible results.
   *
   * @param location The path to the location of the error
   * @param reason  A description of the problem
   * @param resolution  A description of how the problem was resolved
   */
  void error( String[] location, String reason, String resolution);

  /**
   * Returns a message for the given condition arguments.
   */
  default String messageFor( String[] location, String reason, String resolution)
    {
    return
      String.format(
        "%s%s.%s",
        location.length == 0? "" : String.format( "%s: ", StringUtils.join( location, ",")),
        reason,
        StringUtils.isBlank( resolution)? "" : String.format( " %s.", resolution));
    }
  
  /**
   * Returns a {@link Notifier} that ignores all conditions.
   */
  public static Notifier ignore()
    {
    return
      new Notifier()
        {
        @Override
        public void warn( String[] location, String reason) {}
        @Override
        public void error( String[] location, String reason, String resolution) {}
        @Override
        public String toString() {return "IGNORE";}
        };
    }

  /**
   * Returns a {@link Notifier} that logs all conditions, using the given {@link Logger}.
   */
  public static Notifier log( final Logger logger)
    {
    return
      new Notifier()
        {
        @Override
        public void warn( String[] location, String reason)
          {
          logger.warn( messageFor( location, reason, null));
          }
        
        @Override
        public void error( String[] location, String reason, String resolution)
          {
          logger.error( messageFor( location, reason, resolution));
          }

        @Override
        public String toString()
          {
          return "LOG";
          }
        };
    }
  }
