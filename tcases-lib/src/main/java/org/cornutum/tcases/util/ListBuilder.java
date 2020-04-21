//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Provide a fluent List constructor.
 */
public class ListBuilder<T>
  { 
  /**
   * Creates a new ListBuilder object.
   */
  protected ListBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new ListBuilder object.
   */
  protected ListBuilder( List<T> list)
    {
    list_ =
      list == null
      ? new ArrayList<T>()
      : list;
    }

  /**
   * Creates a new ListBuilder object.
   */
  public static <T> ListBuilder<T> to()
    {
    return to( null);
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
   * If present, appends a new element to the list for this builder.
   */
  public ListBuilder<T> add( Optional<T> element)
    {
    element.ifPresent( e -> list_.add( e));
    return this;
    }

  /**
   * Appends all elements of the given collection to the list for this builder.
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
   * Returns the number of elements added to this builder.
   */
  public int size()
    {
    return list_.size();
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
