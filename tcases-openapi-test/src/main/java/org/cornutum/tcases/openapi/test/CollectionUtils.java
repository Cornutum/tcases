//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Defines methods for handling collections.
 */
public final class CollectionUtils
  {
  /**
   * Creates a new CollectionUtils instance.
   */
  private CollectionUtils()
    {
    // Static methods only.
    }


  /**
   * Returns a stream that produces the sequence defined by the given Iterator.
   */
  public static <T> Stream<T> toStream( Iterator<T> iterator)
    {
    return
      Optional.ofNullable( iterator)
      .map( i -> {
        Iterable<T> iterable = () -> i;
        return toStream( iterable);
        })
      .orElse( null);
    }

  /**
   * Returns a stream that produces the sequence defined by the given Iterable.
   */
  public static <T> Stream<T> toStream( Iterable<T> iterable)
    {
    return
      Optional.ofNullable( iterable)
      .map( i -> StreamSupport.stream( i.spliterator(), false))
      .orElse( null);
    }

  /**
   * A collector that produces a map sorted in insertion order.
   */
  public static <T,V> Collector<T,?,Map<String,V>> toOrderedMap( Function<T,String> keyMapper, Function<T,V> valueMapper)
    {
    return
      toMap(
        keyMapper,
        valueMapper,
        (v1, v2) -> v1,
        LinkedHashMap::new);
    }

  /**
   * Returns a new list contains the head elements followed by the tail elements.
   */
  @SafeVarargs
  public static <T> List<T> concatList( List<T> head, T... tail)
    {
    return
      Stream.concat(
        Optional.ofNullable( head).map( List::stream).orElse( Stream.empty()),
        Arrays.stream( tail))
      .collect( toList());
    }

  /**
   * Returns a stream that concatenates the members of the given streams.
   */
  @SafeVarargs
  public static <T> Stream<T> concatStream( Stream<T>... streams)
    {
    return concatStream( Arrays.asList( streams));
    }

  /**
   * Returns a stream that concatenates the members of the given streams.
   */
  public static <T> Stream<T> concatStream( List<Stream<T>> streams)
    {
    return
      streams.isEmpty()
      ? Stream.empty()
      : Stream.concat( streams.get(0), concatStream( streams.subList( 1, streams.size())));
    }

  }
