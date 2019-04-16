//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
  public static <T> Stream<T> toStream( Iterator<T> iterator)
    {
    Iterable<T> iterable = () -> iterator;
    return toStream( iterable);
    }

  /**
   * Returns a stream that produces the sequence defined by the given Iterable.
   */
  public static <T> Stream<T> toStream( Iterable<T> iterable)
    {
    return StreamSupport.stream( iterable.spliterator(), false);
    }

  /**
   * If the given value is present, returns it as a singleton list. Otherwise, returns an empty list.
   */
  public static <T> List<T> iterableOf( Optional<T> value)
    {
    return value.map( Arrays::asList).orElse( Collections.emptyList());
    }

  /**
   * If the given value is present, returns it as a single-element stream. Otherwise, returns an empty stream.
   */
  public static <T> Stream<T> streamOf( Optional<T> value)
    {
    return value.map( Stream::of).orElse( Stream.empty());
    }

  /**
   * Returns a stream containing the members of the given collection.
   */
  public static <T,C extends Collection<T>> Stream<T> membersOf( C collection)
    {
    return
      collection == null
      ? Stream.empty()
      : collection.stream();
    }

  /**
   * Returns a stream containing the entries of the given map.
   */
  public static <K,V,M extends Map<K,V>> Stream<Map.Entry<K,V>> entriesOf( M map)
    {
    return
      map == null
      ? Stream.empty()
      : map.entrySet().stream();
    }
  }
