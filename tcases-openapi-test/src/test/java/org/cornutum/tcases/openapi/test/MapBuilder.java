//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.HashMap;
import java.util.Map;

/**
 * Builds a new Map instance.
 */
public class MapBuilder<K,V>
  {
  /**
   * Creates a new MapBuilder instance.
   */
  public MapBuilder()
    {
    }

  /**
   * Creates a new MapBuilder instance.
   */
  public MapBuilder( Map<? extends K,? extends V> other)
    {
    map_.putAll( other);
    }

  /**
   * Adds an entry to this map.
   */
  public MapBuilder<K,V> put( K key, V value)
    {
    map_.put( key, value);
    return this;
    }

  /**
   * Builds this map.
   */
  public Map<K,V> build()
    {
    return map_;
    }

  private final Map<K,V> map_ = new HashMap<K,V>();
  }
