//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

/**
 * Defines methods to construct standard string representations.
 *
 * @version $Revision$, $Date$
 */
public abstract class ToString
  {
  /**
   * Returns a standard string builder for the given object.
   */
  public static ToStringBuilder getBuilder( Object object)
    {
    return new ToStringBuilder( object, ToStringStyle.SHORT_PREFIX_STYLE);
    }
  }

