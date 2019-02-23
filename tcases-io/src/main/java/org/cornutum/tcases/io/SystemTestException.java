//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

/**
 * Reports an error reading a system test definition.
 */
public class SystemTestException extends RuntimeException
  {
  private static final long serialVersionUID = 1224093815535269784L;

  /**
   * Creates a new SystemTestException instance.
   */
  public SystemTestException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new SystemTestException instance.
   */
  public SystemTestException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
