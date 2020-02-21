//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;

/**
 * Base class for a set of fixed-length string values can be used by a request.
 */
public abstract class RestrictedStringDomain extends AbstractStringDomain
  {  
  /**
   * Creates a new RestrictedStringDomain instance.
   */
  protected RestrictedStringDomain( int maxLength)
    {
    super( maxLength);
    }

  /**
   * Defines a constant length range for values in this domain.
   */
  public void setLengthRange( Integer length)
    {
    if( !getLengthRange().contains( length))
      {
      throw new RequestCaseException( String.format( "Invalid value length=%s -- length must be %s", length, getLengthRange()));
      }
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Integer min, Integer max)
    {
    throw new RequestCaseException( String.format( "Invalid value length range=[%s,%s] -- length must be %s", min, max, getLengthRange()));
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Range range)
    {
    throw new RequestCaseException( String.format( "Invalid value length range=%s -- length must be %s", range, getLengthRange()));
    }
}
