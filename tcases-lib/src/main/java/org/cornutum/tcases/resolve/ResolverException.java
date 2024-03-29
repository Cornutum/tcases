//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.apache.commons.lang3.StringUtils;

/**
 * Reports a failure to resolve a {@link DataValue}.
 */
public class ResolverException extends RuntimeException
  {
  private static final long serialVersionUID = 6745393444958854970L;

  /**
   * Creates a new ResolverException instance.
   */
  public ResolverException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new ResolverException instance.
   */
  public ResolverException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  
  /**
   * Creates a new ResolverException instance.
   */
  public ResolverException( String[] location, Throwable cause)
    {
    super( String.format( "Error processing %s", StringUtils.join( location, ", ")), cause);
    }
  }
