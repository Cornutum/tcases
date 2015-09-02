//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * Iterates of the Cartesian product of a list of sets.
 */
public class CartesianProduct<T> implements Iterator<List<T>> 
  {
  /**
   * Creates a new iterator for the CartesianProduct of the given sets.
   */
  public CartesianProduct( List<? extends Set<T>> sets)
    {
    int setCount = sets.size();

    Set<T> firstSet =
      setCount == 0
      ? Collections.<T>emptySet()
      : sets.get(0);

    firstSetIterator_ = firstSet.iterator();

    otherSets_ =
      setCount < 2
      ? Collections.<Set<T>>emptyList()
      : sets.subList( 1, setCount);
    }

  public boolean hasNext()
    {
    return getNext() != null;
    }

  public List<T> next()
    {
    List<T> next = getNext();
    if( next == null)
      {
      throw new NoSuchElementException();
      }

    next_ = null;
    return next;
    }

  public void remove()
    {
    throw new UnsupportedOperationException();
    }
  
  private List<T> getNext()
    {
    if( next_ == null)
      {
      if( firstSetNext_ != null && otherSetsProduct_.hasNext())
        {
        next_ = new ArrayList<T>();
        next_.add( firstSetNext_);
        next_.addAll( otherSetsProduct_.next());
        }          
      else if( firstSetIterator_.hasNext())
        {
        firstSetNext_ = firstSetIterator_.next();

        otherSetsProduct_ =
            otherSets_.isEmpty()
            ? Arrays.asList( Collections.<T>emptyList()).iterator()
            : new CartesianProduct<T>( otherSets_);

        next_ = getNext();
        }
      }

    return next_;
    }

  private Iterator<T> firstSetIterator_;
  private T firstSetNext_;

  private List<? extends Set<T>> otherSets_;
  private Iterator<? extends List<T>> otherSetsProduct_;

  private List<T> next_;
  }
