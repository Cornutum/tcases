//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.VarDef;

import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;

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
   * Returns input tuples not yet used in a test case that bind the given variable.
   */
  public Iterator<Tuple> getUnused( final VarDef var)
    {
    return
      IteratorUtils.filteredIterator
      ( unused_.iterator(),
        new Predicate<Tuple>()
        {
        public boolean evaluate( Tuple tuple)
          {
          return tuple.getBinding( var) != null;
          }
        });
    }

  /**
   * Returns input tuples already used in a test case.
   */
  public Iterator<Tuple> getUsed()
    {
    return used_.iterator();
    }

  /**
   * Returns input tuples already used in a test case that bind the given variable.
   */
  public Iterator<Tuple> getUsed( final VarDef var)
    {
    return
      IteratorUtils.filteredIterator
      ( used_.iterator(),
        new Predicate<Tuple>()
        {
        public boolean evaluate( Tuple tuple)
          {
          return tuple.getBinding( var) != null;
          }
        });
    }

  /**
   * Asserts that the given tuple has been used in a test case.
   */
  public void used( Tuple tuple)
    {
    // Currently unused?
    int i = unused_.indexOf( tuple);
    if( i >= 0)
      {
      // Yes, relocated to used list.
      used_.add( unused_.remove( i));
      }

    // No, already used?
    else if( (i = used_.indexOf( tuple)) >= 0)
      {
      // Yes, move to the end of the list. This acts to keep the used list
      // in least-recently-used-first order.
      used_.add( used_.remove( i));
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
