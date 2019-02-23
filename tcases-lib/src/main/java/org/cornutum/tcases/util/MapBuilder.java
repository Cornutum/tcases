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
  @SuppressWarnings("unchecked")
  public MapBuilder()
    {
    this( HashMap.class);
    }

  /**
   * Creates a new MapBuilder object.
   */
  public <T extends Map<K,V>> MapBuilder( Class<T> type)
    {
    try
      {
      map_ = type.newInstance();
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( "Can't create instance of type=" + type);
      }
    }

  /**
   * Returns a new MapBuilder with the given entry.
   */
  public static <K,V> MapBuilder<K,V> of( K key, V value)
    {
    return new MapBuilder<K,V>().put( key, value);
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

  private Map<K,V> map_;
  }
