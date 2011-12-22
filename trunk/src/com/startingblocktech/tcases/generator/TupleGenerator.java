//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.lang.ObjectUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
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
    List<TestCase> validCases = getValidCases( inputDef, baseTests);

    FunctionTestDef testDef = new FunctionTestDef( inputDef.getName());
    for( TestCase testCase : validCases)
      {
      testDef.addTestCase( testCase);
      }
    
    return testDef;
    }

  /**
   * Returns a set of valid {@link TestCase test cases} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  private List<TestCase> getValidCases( FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    List<TestCase> validCases = new ArrayList<TestCase>();
    VarTupleSet tuples = getValidTupleSet( inputDef);

    // For each valid input tuple not yet used in a test case...
    Tuple nextUnused;
    for( int testCaseId = 0;
         (nextUnused = tuples.getNextUnused()) != null;
         testCaseId++)
      {
      // Create a new test case.
      TestCase validCase = new TestCase( testCaseId);
      validCases.add( validCase);
      }
    
    return validCases;
    }

  /**
   * Returns the all valid input tuples required for generated test cases.
   */
  private VarTupleSet getValidTupleSet( FunctionInputDef inputDef)
    {
    List<Tuple> validTuples = new ArrayList<Tuple>();
    RandSeq randSeq = getRandomSeed()==null? null : new RandSeq( getRandomSeed());

    // Get tuple sets required for each specified combiner,
    // ordered for "greedy" processing, i.e. biggest tuples first.
    TupleCombiner[] combiners = new TupleCombiner[ getCombiners().size()];
    getCombiners().toArray( combiners);
    Arrays.sort
      ( combiners,
        new Comparator<TupleCombiner>()
          {
          public int compare( TupleCombiner combiner1, TupleCombiner combiner2)
            {
            return combiner2.getTupleSize() - combiner1.getTupleSize();
            }
          });
    for( int i = 0; i < combiners.length; i++)
      {
      validTuples.addAll( RandSeq.order( randSeq, combiners[i].getTuples( inputDef)));
      }

    // For each input variable...
    for( VarDefIterator varDefs = new VarDefIterator( inputDef); varDefs.hasNext(); )
      {
      VarDef varDef = varDefs.next();

      // ... that does not belong to a combiner tuple set...
      int i;
      for( i = 0; i < combiners.length && !combiners[i].isEligible( varDef); i++);
      if( i >= combiners.length)
        {
        // ...add the set of all its 1-tuples.
        validTuples.addAll( RandSeq.order( randSeq, TupleCombiner.getTuples( varDef)));
        }
      }
    
    return new VarTupleSet( validTuples);
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

