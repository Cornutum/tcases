//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver.io;

import org.cornutum.tcases.openapi.resolver.DataValue;

/**
 * Converts a {@link DataValue} into a different form.
 */
public interface DataValueConverter<T>
  {
  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  public T convert( DataValue<?> value);
  }
