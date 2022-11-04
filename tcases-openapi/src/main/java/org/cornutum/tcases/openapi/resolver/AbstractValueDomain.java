//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.resolve.DataValue;

/**
 * Base class for {@link ValueDomain} implementations
 */
public abstract class AbstractValueDomain<T> implements ValueDomain<T>
  {
  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected abstract DataValue<T> dataValueOf( T value);
  }
