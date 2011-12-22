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
   * Returns the next unused tuple from this set. Returns null if all
   * tuples have been used.
   */
  public Tuple getNextUnused()
    {
    Iterator<Tuple> unused = getUnused();
    return
      unused.hasNext()
      ? unused.next()
      : null;
    }

  private List<Tuple> unused_;
  private List<Tuple> used_;
  }
