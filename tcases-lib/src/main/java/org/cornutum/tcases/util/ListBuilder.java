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
 * Provide a fluent List constructor.
 */
public class ListBuilder<T>
  {  
  /**
   * Creates a new ListBuilder object.
   */
  private ListBuilder( List<T> list)
    {
    list_ = list;
    }

  /**
   * Creates a new ListBuilder object.
   */
  public static <T> ListBuilder<T> to()
    {
    return to( new ArrayList<T>());
    }

  /**
   * Creates a new ListBuilder object.
   */
  public static <T> ListBuilder<T> to( List<T> list)
    {
    return new ListBuilder<T>( list);
    }
  
  /**
   * Appends a new element to the list for this builder.
   */
  public ListBuilder<T> add( T element)
    {
    list_.add( element);
    return this;
    }

  /**
   * Appends all elements of the given colletion to the list for this builder.
   */
  public ListBuilder<T> addAll( Iterable<T> elements)
    {
    for( T element : elements)
      {
      list_.add( element);
      }
    
    return this;
    }

  /**
   * Returns the list for this builder.
   */
  public List<T> build()
    {
    return list_;
    }

  private List<T> list_;
  }
