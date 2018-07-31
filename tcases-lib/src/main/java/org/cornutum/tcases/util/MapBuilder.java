//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.Map;
import java.util.Optional;
import java.util.HashMap;

/**
 * Creates a {@link Map} instance.
 */
public class MapBuilder<K,V>
  {
  /**
   * Creates a new MapBuilder object.
   */
  public MapBuilder()
    {
    this( null);
    }

  /**
   * Creates a new MapBuilder object.
   */
  public MapBuilder( Map<K,V> map)
    {
    map_ =
      map == null
      ? new HashMap<K,V>()
      : map;
    }

  /**
   * Returns a new MapBuilder with the given entry.
   */
  public static <K,V> MapBuilder<K,V> of( K key, V value)
    {
    return new MapBuilder<K,V>().put( key, value);
    }

  /**
   * Returns a Map with the given entry as an Optional. Returns <CODE>Optional.empty()</CODE> if
   * the given value is not present.
   */
  public static <K,V> Optional<Map<K,V>> optionalOf( K key, Optional<V> value)
    {
    return new MapBuilder<K,V>().putIf( key, value).optional();
    }

  /**
   * Adds an entry to the map for this builder.
   */
  public MapBuilder<K,V> put( K key, V value)
    {
    map_.put( key, value);
    return this;
    }

  /**
   * Adds an entry to the map for this builder if the given value is present.
   */
  public MapBuilder<K,V> putIf( K key, Optional<V> value)
    {
    value.ifPresent( v -> map_.put( key, v));
    return this;
    }

  /**
   * Returns the Map for this builder.
   */
  public Map<K,V> build()
    {
    return map_;
    }

  /**
   * Returns the Map for this builder as an Optional. Returns <CODE>Optional.empty()</CODE> if the Map is empty.
   */
  public Optional<Map<K,V>> optional()
    {
    return
      map_.isEmpty()
      ? Optional.empty()
      : Optional.of( map_);
    }

  private Map<K,V> map_;
  }
