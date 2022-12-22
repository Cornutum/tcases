//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

/**
 * Reports a failure using a {@link ValueDomain}.
 */
public class ValueDomainException extends RuntimeException
  {
  private static final long serialVersionUID = -8286825502237253144L;

  /**
   * Creates a new ValueDomainException instance.
   */
  public ValueDomainException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new ValueDomainException instance.
   */
  public ValueDomainException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
