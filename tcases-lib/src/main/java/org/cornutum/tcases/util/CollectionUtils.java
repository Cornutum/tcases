//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import org.apache.commons.collections4.Predicate;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
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

  /**
   * Returns the given collection after removing any member that does not satisfy the given predicate.
   */
  static public <T,C extends Iterable<T>> C filtered( C collection, Predicate<T> predicate)
    {
    for( Iterator<T> members = collection.iterator(); members.hasNext(); )
      {
      if( !predicate.evaluate( members.next()))
        {
        members.remove();
        }
      }

    return collection;
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
