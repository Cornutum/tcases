//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.apache.commons.collections4.IteratorUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

/**
 * Returns a random permutation of a sequence.
 *
 */
public class RandSeq
  {
  /**
   * Creates a new RandSeq object.
   */
  public RandSeq( Random generator)
    {
    generator_ = generator;
    }
  
  /**
   * Creates a new RandSeq object.
   */
  public RandSeq()
    {
    this( new Random());
    }
  
  /**
   * Creates a new RandSeq object.
   */
  public RandSeq( long seed)
    {
    this( new Random( seed));
    }

  /**
   * Returns the given list with its elements rearranged in a random order.
   */
  public <T> List<T> reorder( List<T> sequence)
    {
    int elementCount = sequence.size();
    if( elementCount > 1)
      {
      List<T> elements = new ArrayList<T>( sequence);
      sequence.clear();

      for( ; elementCount > 0; elementCount--)
        {
        sequence.add( elements.remove( generator_.nextInt( elementCount)));
        }
      }

    return sequence;
    }

  /**
   * Returns an iterator that visits the elements of the given sequence in a random order.
   */
  public <T> Iterator<T> reorder( Iterator<T> sequence)
    {
    return reorder( IteratorUtils.toList( sequence)).iterator();
    }

  /**
   * Applies the given <code>randSeq</code> to rearrange the given sequence in a random order.
   * If <code>randSeq</code> is <code>null</code>, returns the sequence unchanged.
   */
  public static <T> List<T> reorderIf( RandSeq randSeq, List<T> sequence)
    {
    return randSeq==null? sequence : randSeq.reorder( sequence);
    }

  /**
   * Applies the given <code>randSeq</code> to rearrange the given collection in a random order.
   * If <code>randSeq</code> is <code>null</code>, returns the collection in normal order.
   */
  public static <T> List<T> order( RandSeq randSeq, Collection<T> collection)
    {
    return reorderIf( randSeq, new ArrayList<T>( collection));
    }

  /**
   * Applies the given <code>randSeq</code> to rearrange the given sequence in a random order.
   * If <code>randSeq</code> is <code>null</code>, returns the sequence unchanged.
   */
  public static <T> Iterator<T> reorderIf( RandSeq randSeq, Iterator<T> sequence)
    {
    return randSeq==null? sequence : randSeq.reorder( sequence);
    }

  private Random generator_;
  }

