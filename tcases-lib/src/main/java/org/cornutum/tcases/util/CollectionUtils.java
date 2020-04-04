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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toCollection;
import static java.util.stream.Collectors.toList;

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

  /**
   * Returns the given list of values as a comma-separated string.
   */
  public static String toCsv( Stream<?> values)
    {
    return
      values
      .map( value ->
            Optional.ofNullable( value)
            .map( v -> String.format( "'%s'", v.toString().replace( "\\", "\\\\").replace( "'", "\\'")))
            .orElse( "null"))
      .collect( joining( ","));
    }

  /**
   * Returns the list of values specified by the given comma-separated string.
   * @see #toCsv
   */
  public static Stream<String> fromCsv( String csv)
    {
    Stream.Builder<String> values = Stream.builder();

    if( csv != null)
      {
      int length = csv.length();
      for( int i = 0; i < length; i++)
        {
        if( csv.startsWith( "null", i))
          {
          values.add( null);
          i += "null".length();
          }
        else if( csv.charAt(i) == '\'')
          {
          StringBuilder value = new StringBuilder();
          for( i++; i < length && csv.charAt(i) != '\''; i++)
            {
            if( csv.charAt( i) == '\\')
              {
              i++;
              }

            if( i < length)
              {
              value.append( csv.charAt( i));
              }
            }

          values.add( value.toString());
          i++;
        }
        else
          {
          throw new IllegalArgumentException( String.format( "Invalid CSV: value at index=%s is neither null nor a quoted string", i));
          }
        }
      }
    
    return values.build();
    }

  /**
   * Returns a {@link Collector} that accumulates set elements in insertion order
   */
  public static <T> Collector<T,?,Set<T>> toOrderedSet()
    {
    return toCollection( LinkedHashSet::new);
    }

  /**
   * Returns a set containing the given elements in iteration order.
   */
  public static <T> Set<T> asOrderedSet( Iterable<T> elements)
    {
    return
      Optional.ofNullable( elements).map( CollectionUtils::toStream).orElse( Stream.empty())
      .collect( toOrderedSet());
    }

  /**
   * Returns a set containing the given elements in iteration order.
   */
  @SafeVarargs
  public static <T> Set<T> asOrderedSet( T... elements)
    {
    return asOrderedSet( Arrays.asList( elements));
    }

  /**
   * Returns a list of all given members except for the one at the excluded position.
   */
  public static <T> List<T> restOf( List<T> members, int excluded)
    {
    return
      IntStream.range( 0, members.size())
      .filter( i -> i != excluded)
      .mapToObj( i -> members.get(i))
      .collect( toList());
    }
}
