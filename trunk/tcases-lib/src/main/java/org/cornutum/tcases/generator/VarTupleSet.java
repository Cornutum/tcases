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

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;

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
   * Returns input tuples not yet used in a test case that bind any of the given variables.
   */
  public Iterator<Tuple> getUnused( final List<VarDef> vars)
    {
    return
      IteratorUtils.filteredIterator
      ( unused_.iterator(),
        new Predicate<Tuple>()
        {
        public boolean evaluate( Tuple tuple)
          {
          boolean binds;
          Iterator<VarDef> bindVars;
          for( binds = false,
                 bindVars = vars.iterator();
               
               !binds && bindVars.hasNext();
               
               binds = tuple.getBinding( bindVars.next()) != null);
          
          return binds;
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
   * Returns input tuples already used in a test case that bind any of the given variables.
   * Returns only tuples for which <CODE>isOnce() == once</CODE>.
   */
  public Iterator<Tuple> getUsed( final List<VarDef> vars, final boolean once)
    {
    return
      IteratorUtils.filteredIterator
      ( used_.iterator(),
        new Predicate<Tuple>()
          {
          public boolean evaluate( Tuple tuple)
            {
            boolean binds = false;
            if( tuple.isOnce() == once)
              {
              for( Iterator<VarDef> bindVars = vars.iterator();
                   !binds && bindVars.hasNext();
                   binds = tuple.getBinding( bindVars.next()) != null);
              }
            return binds;
            }
          });
    }

  /**
   * Asserts use of all tuples contained in the given test case.
   */
  public void used( final TestCaseDef testCase)
    {
    List<Tuple> usedTuples =
      IteratorUtils.toList
      ( IteratorUtils.filteredIterator
        ( unused_.iterator(),
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
      used_.add( tuple);

      if( tuple.size() > 1)
        {
        // Once used, N-tuples can be reduced to 1-tuples.  This enables different combinations
        // that may be required to complete tests for other tuples. In particular, it allows for an
        // NA binding of an optional variable, which will never appear in N-tuples.
        for( Iterator<VarBindingDef> bindings = tuple.getBindings(); bindings.hasNext();)
          {
          Tuple tuple1 = new Tuple( bindings.next());
          tuple1.setOnce( tuple.isOnce());
          if( used_.indexOf( tuple1) < 0)
            {
            used_.add( tuple1);
            }
          }
        }
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
  }
