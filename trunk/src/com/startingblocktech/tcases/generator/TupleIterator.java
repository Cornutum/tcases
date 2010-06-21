//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.util.ToString;
import com.startingblocktech.tcases.VarDef;
import com.startingblocktech.tcases.VarValueDef;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Iterates over all N-tuples of compatible values for a given set of input variables.
 *
 * @version $Revision$, $Date$
 */
public class TupleIterator implements Iterator<Tuple>
  {
  /**
   * Creates a new TupleIterator object.
   */
  public TupleIterator( int tupleSize, List<VarDef> varDefs)
    {
    setTupleSize( tupleSize);
    varDefs_ = varDefs;

    varStart_ = -1;
    varEnd_ = varDefs.size() - tupleSize + 1;
    if( tupleSize < 1 || varEnd_ <= 0)
      {
      throw new IllegalArgumentException( "Can't create " + tupleSize + "-tuples for a list of size=" + varDefs.size());
      }
    }

  public boolean hasNext()
    {
    return getNextCompatibleTuple() != null;
    }

  public Tuple next()
    {
    Tuple nextTuple = getNextCompatibleTuple();
    if( nextTuple == null)
      {
      throw new NoSuchElementException();
      }

    nextTuple_ = null;
    return nextTuple;
    }

  public void remove()
    {
    throw new UnsupportedOperationException();
    }

  /**
   * Changes the size of tuples returned.
   */
  private void setTupleSize( int tupleSize)
    {
    tupleSize_ = tupleSize;
    }

  /**
   * Returns the size of tuples returned.
   */
  public int getTupleSize()
    {
    return tupleSize_;
    }

  private Tuple getNextCompatibleTuple()
    {
    Tuple nextTuple;
    while( !((nextTuple = getNextTuple()) == null || nextTuple.isCompatible()))
      {
      nextTuple_ = null;
      }

    return nextTuple;
    }

  /**
   * Returns the next N-tuple of compatible values.
   */
  private Tuple getNextTuple()
    {
    if( nextTuple_ == null)
      {
      // Still traversing values for current start variable?
      int tupleSize = getTupleSize();
      if( !(values_ != null && values_.hasNext()))
        {
        // No, still traversing subtuples for current start variable?
        values_ = null;
        subTuple_ = null;
        if( !(subTuples_ != null && subTuples_.hasNext()))
          {
          // No, advance to next start variable.
          subTuples_ = null;
          varStart_++;
          if( tupleSize > 1)
            {
            for( ;
                 varStart_ < varEnd_
                   &&
                   (subTuples_ = new TupleIterator( tupleSize - 1, varDefs_.subList( varStart_ + 1, varDefs_.size())))
                   .hasNext() == false;

                 varStart_++);
            }
          }
        }

      // More tuples remaining?
      if( varStart_ < varEnd_)
        {
        VarDef startVarDef = varDefs_.get( varStart_);
        if( values_ == null)
          {
          // Resume traversing values for current start variable.
          values_ = startVarDef.getValidValues();
          if( tupleSize > 1)
            {
            // Advance to next subtuple for current start variable.
            subTuple_ = subTuples_.next();
            }
          }

        nextTuple_ = new Tuple();
        if( values_.hasNext())
          {
          nextTuple_.add( new VarBindingDef( startVarDef, values_.next()));
          }
        if( subTuple_ != null)
          {
          nextTuple_.addAll( subTuple_);
          }
        }
      }

    return nextTuple_;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "size", getTupleSize())
      .toString();
    }

  private int                   tupleSize_;
  private List<VarDef>          varDefs_;
  private int                   varStart_;
  private int                   varEnd_;
  private Iterator<VarValueDef> values_;
  private Tuple                 subTuple_;
  private TupleIterator         subTuples_;
  private Tuple                 nextTuple_;
  }

