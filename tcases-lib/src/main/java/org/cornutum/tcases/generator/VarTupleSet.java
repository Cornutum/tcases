//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * Defines a set of input variable combinations used for test cases.
 *
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
  public Iterator<Tuple> getUnused( VarDef var)
    {
    return getBinds( unused_.iterator(), var);
    }

  /**
   * Returns a measure of the "unused-ness" of the given binding.
   * Returns a value between 0 and 1 (exclusive) -- a higher value means a binding is more unused.
   */
  public double getUnusedScore( final VarBindingDef binding)
    {
    // Returns the percentage of unused tuples that include this binding.
    return
      unused_.isEmpty()
      ? 0.0

      : (double)
        IterableUtils.countMatches
        ( unused_,
          new Predicate<Tuple>()
            {
            public boolean evaluate( Tuple tuple)
              {
              return tuple.contains( binding);
              }
            })
        / unused_.size();
    }

  /**
   * Returns input tuples already used in a test case.
   */
  public Iterator<Tuple> getUsed()
    {
    if( usedChanged_)
      {
      // Order to prefer reuse of larger tuples first. By preferring "intact" tuples over post-reduction singletons,
      // we hope to minimize reoccurrence of once-only tuples.
      Collections.sort( used_, tupleSizeDescending_);
      usedChanged_ = false;
      }
    
    return used_.iterator();
    }

  /**
   * Returns input tuples already used in a test case that bind the given variable,
   * excluding once-only tuples.
   */
  public Iterator<Tuple> getUsed( VarDef var)
    {
    return getUsed( var, false);
    }

  /**
   * Returns once-only input tuples already used in a test case that bind the given variable.
   */
  public Iterator<Tuple> getUsedOnce( VarDef var)
    {
    return getUsed( var, true);
    }

  /**
   * Returns input tuples already used in a test case that bind the given variable, considering
   * only tuples that are (not) once-only
   */
  public Iterator<Tuple> getUsed( VarDef var, final boolean onceOnly)
    {
    return
      getBinds
      ( IteratorUtils.filteredIterator
        ( getUsed(),
          new Predicate<Tuple>()
            {
            public boolean evaluate( Tuple tuple)
              {
              return tuple.isOnce() == onceOnly;
              }
            }),
        var);
    }

  /**
   * Returns a measure of the "used-ness" of the given binding.
   * Returns a value between 0 and 1 (exclusive) -- a higher value means a binding is more used.
   */
  public double getUsedScore( VarBindingDef binding)
    {
    return getUsedScore( binding, false);
    }

  /**
   * Returns a measure of the "used-ness" of the given binding among once-only tuples.
   * Returns a value between 0 and 1 (exclusive) -- a higher value means a binding is more used among once-only tuples.
   */
  public double getUsedOnceScore( VarBindingDef binding)
    {
    return getUsedScore( binding, true);
    }

  /**
   * Returns a measure of the "used-ness" of the given binding. If <CODE>onceOnly</CODE> is true, consider only once-only tuples.
   * Returns a value between 0 and 1 (exclusive) -- a higher value means a binding is more used.
   */
  private double getUsedScore( VarBindingDef binding, boolean onceOnly)
    {
    // Since used tuples are in least-recently-used-first order, start from the end of the list and look
    // for the first tuple that includes this binding. This is the most recent use of this binding.
    // Return the distance of this tuple from the start of the list, as percentage of the size of the list.
    int index;
    int usedCount;
    Tuple nextTuple;
    for( usedCount = used_.size(),
           index = usedCount - 1;

         index >= 0
           && (nextTuple = used_.get( index)) != null
           && !((!onceOnly || nextTuple.isOnce()) && nextTuple.contains( binding));

         index--);

    return
      index < 0
      ? 0.0

      : (double)(index + 1) / (usedCount + 1);
    }

  /**
   * Returns input tuples that bind the given variable.
   */
  private Iterator<Tuple> getBinds( Iterator<Tuple> tuples, final VarDef var)
    {
    return
      IteratorUtils.filteredIterator
      ( tuples,
        new Predicate<Tuple>()
          {
          public boolean evaluate( Tuple tuple)
            {
            return tuple.getBinding( var) != null;
            }
          });
    }

  /**
   * Asserts use of all tuples contained in the given test case.
   */
  public void used( final TestCaseDef testCase)
    {
    // Note: must accumulate tuples into a separate list to avoid ConcurrentModificationException when updating used/unused membership.
    List<Tuple> usedTuples =
      IteratorUtils.toList
      ( IteratorUtils.filteredIterator
        ( IteratorUtils.chainedIterator
          ( unused_.iterator(),
            used_.iterator()),
          new Predicate<Tuple>()
            {
            public boolean evaluate( Tuple tuple)
              {
              return testCase.usesTuple( tuple);
              }
            }));

    for( Tuple tuple : usedTuples)
      {
      used( tuple);
      }
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
      // Yes, relocate to used list. 
      unused_.remove( i);
      addUsed( tuple);

      if( tuple.size() > 1)
        {
        // Once used, N-tuples can be reduced to 1-tuples.  This enables different combinations
        // that may be required to complete tests for other tuples. In particular, it allows for an
        // NA binding of an optional variable, which will never appear in N-tuples.
        for( Iterator<VarBindingDef> bindings = tuple.getBindings(); bindings.hasNext();)
          {
          Tuple tuple1 = new Tuple( bindings.next());
          tuple1.setOnce( tuple.isOnce());

          // 1-tuple already used?
          if( (i = used_.indexOf( tuple1)) >= 0)
            {
            // Yes, move to the end of the list. This acts to keep the used list
            // in least-recently-used-first order.
            used_.remove( i);
            }
          addUsed( tuple1);
          }
        }
      }

    // No, already used?
    else if( (i = used_.indexOf( tuple)) >= 0)
      {
      // Yes, move to the end of the list. This acts to keep the used list
      // in least-recently-used-first order.
      addUsed( used_.remove( i));
      }
    }

  /**
   * Add the given tuple to the list of those used in a test case.
   */
  private void addUsed( Tuple tuple)
    {
    used_.add( tuple);
    usedChanged_ = true;
    }

  /**
   * Removes the given tuple from use in test cases.
   */
  public void remove( Tuple tuple)
    {
    int i = unused_.indexOf( tuple);
    if( i >= 0)
      {
      unused_.remove( tuple);
      }
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

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "used", used_.size())
      .append( "unused", unused_.size())
      .toString();
    }

  private List<Tuple> unused_;
  private List<Tuple> used_;
  private boolean usedChanged_;

  private static final Comparator<Tuple> tupleSizeDescending_ =
    new Comparator<Tuple>()
    {
      public int compare( Tuple tuple1, Tuple tuple2)
        {
         return tuple2.size() - tuple1.size();
        }
    };
  }
