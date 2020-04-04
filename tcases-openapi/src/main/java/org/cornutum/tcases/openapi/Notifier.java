//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.apache.commons.lang3.StringUtils;

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
        "%s: %s.%s",
        StringUtils.join( location, ","),
        reason,
        StringUtils.isBlank( resolution)? "" : String.format( " %s.", resolution));
    }
  }
