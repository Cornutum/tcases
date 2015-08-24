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
import java.util.Collection;
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
   * Returns input tuples not yet used that contain any of the given variable bindings.
   */
  public Iterator<Tuple> getUnused( Collection<VarBindingDef> bindings)
    {
    return getBinds( unused_.iterator(), bindings);
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
   * Returns input tuples already used in a test case that bind the given variable.
   */
  public Iterator<Tuple> getUsed( VarDef var)
    {
    return getBinds( used_.iterator(), var);
    }

  /**
   * Returns input tuples already used that contain any of the given variable bindings.
   */
  public Iterator<Tuple> getUsed( Collection<VarBindingDef> bindings)
    {
    return getBinds( used_.iterator(), bindings);
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
   * Returns input tuples that contain any of the given variable bindings.
   */
  private Iterator<Tuple> getBinds( Iterator<Tuple> tuples, final Collection<VarBindingDef> bindingDefs)
    {
    return
      IteratorUtils.filteredIterator
      ( tuples,
        new Predicate<Tuple>()
          {
          public boolean evaluate( Tuple tuple)
            {
            boolean binds;
            Iterator<VarBindingDef> bindings;
            for( binds = false,
                   bindings = bindingDefs.iterator();

                 !binds
                   && bindings.hasNext();
                 )
              {
              VarBindingDef binding = bindings.next();
              binds = binding.getValueDef().equals( tuple.getBinding( binding.getVarDef()));
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
          if( used_.indexOf( tuple1) < 0)
            {
            addUsed( tuple1);
            }
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
