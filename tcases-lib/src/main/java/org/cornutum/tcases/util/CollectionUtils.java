//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.ArrayList;
import java.util.List;

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
    }

  /**
   * Returns a list containing a clone of each member of the given collection.
   */
  static public <T extends Cloneable<T>> List<T> clonedList( Iterable<T> list)
    {
    List<T> other = new ArrayList<T>();
    for( T member : list)
      {
      other.add( member.cloneOf());
      }
    
    return other;
    }
  }
