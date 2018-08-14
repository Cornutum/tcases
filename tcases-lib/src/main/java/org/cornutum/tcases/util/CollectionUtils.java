//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Defines methods for handling collections.
 */
public final class CollectionUtils
  {
  /**
   * Creates a new CollectionUtils object.
   */
  private CollectionUtils()
    {
    // Static methods only
    }

  /**
   * Returns a stream that produces the sequence defined by the given Iterator.
   */
  static public <T> Stream<T> toStream( Iterator<T> iterator)
    {
    Iterable<T> iterable = () -> iterator;
    return StreamSupport.stream( iterable.spliterator(), false);
    }
  }
