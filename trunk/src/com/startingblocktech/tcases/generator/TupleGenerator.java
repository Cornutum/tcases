//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.lang.ObjectUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
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
    List<TestCaseDef> validCases = getValidCases( inputDef, baseTests);

    FunctionTestDef testDef = new FunctionTestDef( inputDef.getName());
    int id = 0;
    for( TestCaseDef testCase : validCases)
      {
      testDef.addTestCase( testCase.createTestCase( id++));
      }
    
    return testDef;
    }

  /**
   * Returns a set of valid {@link TestCaseDef test case definitions} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  private List<TestCaseDef> getValidCases( FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    List<TestCaseDef> validCases = new ArrayList<TestCaseDef>();
    VarTupleSet tuples = getValidTupleSet( inputDef);

    // For each valid input tuple not yet used in a test case...
    Tuple nextUnused;
    while( (nextUnused = tuples.getNextUnused()) != null)
      {
      // Create a new test case for this tuple.
      TestCaseDef validCase = new TestCaseDef();
      try
        {
        validCase.addBindings( nextUnused);
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't initialize new test case", e);
        }

      // Complete bindings for remaining variables.
      if( !completeBindings( validCase, tuples, getRemainingVars( inputDef, validCase), 0))
        {
        throw new RuntimeException( "Can't create test case for tuple=" + nextUnused);
        }
      
      validCases.add( validCase);
      }
    
    return validCases;
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables,
   * starting with the given index. Returns true if all variables have been bound.
   */
  private boolean completeBindings( TestCaseDef testCase, VarTupleSet tuples, VarDef[] vars, int start)
    {
    // All variables bound?
    boolean complete = start >= vars.length;
    if( !complete)
      {
      // No, look for a compatible binding for the next variable.
      VarDef nextVar = vars[ start];
      Tuple tuple;
      Iterator<Tuple> varTuples;
      for( tuple = null, varTuples = tuples.getUnused( nextVar);
           tuple == null && varTuples.hasNext();
           )
        {
        try
          {
          tuple = varTuples.next();
          testCase.addBindings( tuple);
          }
        catch( BindingException be)
          {
          // TBD: log this event.
          tuple = null;
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't add binding for var=" + nextVar, e);
          }
        }
      }
    
    return complete;
    }

  /**
   * Returns the set of input variables not yet bound by the given test case.
   */
  private VarDef[] getRemainingVars( FunctionInputDef inputDef, final TestCaseDef testCase)
    {
    return
      IteratorUtils.toArray
      ( IteratorUtils.filteredIterator
        ( new VarDefIterator( inputDef),
          new Predicate<VarDef>()
            {
            public boolean evaluate( VarDef var)
              {
              return testCase.getBinding( var) == null;
              }
            }));
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

