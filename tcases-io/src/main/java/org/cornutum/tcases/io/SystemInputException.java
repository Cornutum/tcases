//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

/**
 * Reports an error reading a system input definition.
 */
public class SystemInputException extends RuntimeException
  {
  private static final long serialVersionUID = -8380882050445330505L;

  /**
   * Creates a new SystemInputException instance.
   */
  public SystemInputException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new SystemInputException instance.
   */
  public SystemInputException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
