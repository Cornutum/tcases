//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Defines a set of input variable combinations used for test cases.
 *
 * @version $Revision$, $Date$
 */
public class VarTupleSet
  {
  /**
   * Creates a new VarTupleSet object.
   */
  public VarTupleSet( List<Tuple> tuples)
    {
    unused_ = new ArrayList<Tuple>();
    if( tuples != null)
      {
      unused_.addAll( tuples);
      tupleSize_ = unused_.get(0).size();
      }
    
    used_ = new ArrayList<Tuple>();
    }

  /**
   * Returns input tuples not yet used in a test case.
   */
  public Iterator<Tuple> getUnused()
    {
    return unused_.iterator();
    }

  /**
   * Returns input tuples already used in a test case.
   */
  public Iterator<Tuple> getUsed()
    {
    return used_.iterator();
    }

  /**
   * Returns the tuple size of members of this set.
   */
  public int getTupleSize()
    {
    return tupleSize_;
    }

  /**
   * Asserts that the given tuple has been used in a test case.
   */
  public void used( Tuple tuple)
    {
    int i = unused_.indexOf( tuple);
    if( i >= 0)
      {
      used_.add( unused_.remove( i));
      }
    }

  /**
   * Returns true if all members of this set have been used in test cases.
   */
  public boolean isConsumed()
    {
    return unused_.isEmpty();
    }

  /**
   * Returns the next unused tuple from the given tuple sets.
   */
  public static Tuple getNextUnused( List<VarTupleSet> sets)
    {
    Iterator<VarTupleSet> tupleSets;
    VarTupleSet unconsumed;
    for( tupleSets = sets.iterator(),
           unconsumed = null;
         
         tupleSets.hasNext()
           && (unconsumed = tupleSets.next()).isConsumed();

         unconsumed = null);
    
    return
      unconsumed == null
      ? null
      : unconsumed.getUnused().next();
    }

  private List<Tuple> unused_;
  private List<Tuple> used_;
  private int tupleSize_ = 0;
  }
