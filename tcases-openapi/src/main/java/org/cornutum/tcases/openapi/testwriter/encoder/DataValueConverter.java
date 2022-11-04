//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.resolve.DataValue;

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
