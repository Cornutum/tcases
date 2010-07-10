//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.collections15.ListUtils;
import org.apache.commons.collections15.Transformer;
import org.apache.commons.lang.ObjectUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Generates {@link TestCase test cases} for a {@link FunctionInputDef function} that use
 * all specified N-tuples of valid variable values.
 *
 * @version $Revision$, $Date$
 */
public class TupleGenerator implements ITestCaseGenerator
  {
  /**
   * Creates a new TupleGenerator that, by default, uses all 1-tuples of valid variable values.
   */
  public TupleGenerator()
    {
    this( 1);
    }
  
  /**
   * Creates a new TupleGenerator that, by default, uses all N-tuples of valid variable values.
   */
  public TupleGenerator( int tupleSize)
    {
    setDefaultTupleSize( tupleSize);
    setCombiners( null);
    }

  /**
   * Changes the default tuple size for variable value combinations. A non-positive tupleSize specifies
   * all permutations.
   */
  public void setDefaultTupleSize( int tupleSize)
    {
    defaultTupleSize_ = tupleSize;
    }

  /**
   * Returns the default tuple size for variable value combinations. A non-positive tupleSize specifies
   * all permutations.
   */
  public int getDefaultTupleSize()
    {
    return defaultTupleSize_;
    }

  /**
   * Changes the list of {@link TupleCombiner tuple combiners} for this generator.
   */
  public void setCombiners( List<TupleCombiner> combiners)
    {
    combiners_ = new ArrayList<TupleCombiner>();
    if( combiners != null)
      {
      combiners_.addAll( combiners);
      }
    }

  /**
   * Returns the list of {@link TupleCombiner tuple combiners} for this generator.
   */
  public List<TupleCombiner> getCombiners()
    {
    return combiners_;
    }

  /**
   * Add to the list of {@link TupleCombiner tuple combiners} for this generator.
   */
  public void addCombiner( TupleCombiner combiner)
    {
    combiners_.add( combiner);
    }

  /**
   * Changes the random number sequence seed for this generator.
   */
  public void setRandomSeed( Long seed)
    {
    seed_ = seed;
    }

  /**
   * Returns the random number sequence seed for this generator.
   */
  public Long getRandomSeed()
    {
    return seed_;
    }

  /**
   * Returns a set of {@link TestCase test cases} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  public FunctionTestDef getTests( FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    try
      {
      assertCombinersValid( inputDef);
      return null;
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't generate tests for " + inputDef, e);
      }
    }

  /**
   * Throws an exception if this set of combiners cannot be applied to the given function input
   * definition.
   */
  private void assertCombinersValid( FunctionInputDef inputDef)
    {
    List<TupleCombiner> combiners = getCombiners();

    final FunctionInputDef combinerInputDef = inputDef;

    List<List<VarDef>> combinerVars =
      ListUtils.transformedList
      ( getCombiners(),
        new Transformer<TupleCombiner,List<VarDef>>()
          {
          public List<VarDef> transform( TupleCombiner combiner)
            {
            return combiner.getCombinedVars( combinerInputDef);
            }
          });

    // For every variable...
    int combinerCount = combiners.size();
    for( VarDefIterator varDefs = new VarDefIterator( inputDef); varDefs.hasNext(); )
      {
      VarDef varDef = varDefs.next();

      // Is this variable included in a TupleCombiner?
      int c1;
      for( c1 = 0; c1 < combinerCount && !combinerVars.get( c1).contains( varDef); c1++);
      if( c1 < combinerCount)
        {
        // Yes, but is it also included in another TupleCombiner?
        int c2;
        for( c2 = c1 + 1; c2 < combinerCount && !combinerVars.get( c2).contains( varDef); c2++);
        if( c2 < combinerCount)
          {
          // Yes, report error.
          throw
            new IllegalStateException
            ( "Variable=" + varDef
              + " belongs to multiple combiners=[" + combiners.get( c1) + "," + combiners.get( c2) + "]");
          }
        }
      } 
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "defaultTuples", getDefaultTupleSize())
      .append( "seed", getRandomSeed())
      .toString();
    }

  public boolean equals( Object object)
    {
    TupleGenerator other =
      object != null && object.getClass().equals( getClass())
      ? (TupleGenerator) object
      : null;

    return
      other != null
      && ObjectUtils.equals( other.getRandomSeed(), getRandomSeed())
      && other.getDefaultTupleSize() == getDefaultTupleSize()
      && other.getCombiners().equals( getCombiners()); 
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ (getRandomSeed()==null? 0 : getRandomSeed().hashCode())
      ^ getDefaultTupleSize()
      ^ getCombiners().hashCode();
    }
  
  private Long        seed_;
  private int         defaultTupleSize_;
  List<TupleCombiner> combiners_;
  }

