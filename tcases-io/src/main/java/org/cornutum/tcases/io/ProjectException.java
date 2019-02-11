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
public class ProjectException extends RuntimeException
  {
  private static final long serialVersionUID = -4028271519428016344L;

  /**
   * Creates a new ProjectException instance.
   */
  public ProjectException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new ProjectException instance.
   */
  public ProjectException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
