//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

/**
 * Reports an error in a {@link MocoTestConfig}.
 */
public class MocoTestConfigException extends RuntimeException
  {
  private static final long serialVersionUID = 5482636174761661287L;

  /**
   * Creates a new MocoTestConfigException instance.
   */
  public MocoTestConfigException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new MocoTestConfigException instance.
   */
  public MocoTestConfigException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
