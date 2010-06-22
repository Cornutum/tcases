//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import org.apache.commons.collections15.IteratorUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

/**
 * Returns a random permutation of a sequence.
 *
 * @version $Revision$, $Date$
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

  private Random generator_;
  }

