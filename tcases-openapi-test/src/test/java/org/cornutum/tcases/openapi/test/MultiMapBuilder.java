//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2024, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Builds a new Map that maps a key to a list of values.
 */
public class MultiMapBuilder<K,V> 
  {
  /**
   * Creates a new MultiMapBuilder instance.
   */
  public MultiMapBuilder()
    {
    }

  /**
   * Creates a new MultiMapBuilder instance.
   */
  public MultiMapBuilder( Map<? extends K,? extends V> other)
    {
    other.keySet().forEach( k -> put( k, other.get( k)));
    }

  /**
   * Adds to the list of values for the given key.
   */
  public MultiMapBuilder<K,V> put( K key, V value)
    {
    if( !map_.containsKey( key))
      {
      map_.put( key, new ArrayList<V>());
      }

    map_.get( key).add( value);
    return this;
    }

  /**
   * Builds this map.
   */
  public Map<K,List<V>> build()
    {
    return map_;
    }

  private final Map<K,List<V>> map_ = new HashMap<K,List<V>>();
  }
