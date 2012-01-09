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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
    try
      {
      logger_.info( "{}: generating test cases", inputDef);

      RandSeq randSeq = getRandomSeed()==null? null : new RandSeq( getRandomSeed());
      List<TestCaseDef> validCases = getValidCases( randSeq, inputDef, baseTests);
      List<TestCaseDef> failureCases = getFailureCases( randSeq, inputDef, baseTests, validCases);

      FunctionTestDef testDef = new FunctionTestDef( inputDef.getName());

      // Create test cases, in order of increasing id.
      List<TestCaseDef> testCaseDefs = new ArrayList<TestCaseDef>();
      testCaseDefs.addAll( validCases);
      testCaseDefs.addAll( failureCases);
      Collections.sort( testCaseDefs);

      int nextId = 0;
      for( TestCaseDef testCase : testCaseDefs)
        {
        Integer id = testCase.getId();
        testDef.addTestCase
          ( testCase.createTestCase
            ( id==null? nextId++ : id.intValue()));
        }
    
      logger_.info( "{}: completed {} test cases", inputDef, testCaseDefs.size());
      return testDef;
      }
    catch( Exception e)
      {
      logger_.error( String.valueOf( inputDef) + ": can't create test cases", e);
      throw new RuntimeException( String.valueOf( inputDef) + ": can't create test cases", e);
      }
    }

  /**
   * Returns a set of valid {@link TestCaseDef test case definitions} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  private List<TestCaseDef> getValidCases( RandSeq randSeq, FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    logger_.debug( "{}: creating valid test cases", inputDef);
    
    List<TestCaseDef> validCases = new ArrayList<TestCaseDef>();
    VarTupleSet tuples = getValidTupleSet( randSeq, inputDef);

    // For each valid input tuple not yet used in a test case...
    Tuple nextUnused;
    while( (nextUnused = tuples.getNextUnused()) != null)
      {
      // Create a new test case for this tuple.
      logger_.debug( "Creating test case for tuple={}", nextUnused);

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
      
      logger_.debug( "Completed test case={}", validCase);
      tuples.used( nextUnused);
      validCases.add( validCase);
      }

    logger_.info( "{}: created {} valid test cases", inputDef, validCases.size());
    return validCases;
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables,
   * starting with the given index. Returns true if all variables have been bound.
   */
  private boolean completeBindings( TestCaseDef testCase, VarTupleSet tuples, VarDef[] vars, int start)
    {
    // All variables bound?
    boolean complete = false;
    if( start >= vars.length)
      {
      // Yes, all conditions satisified?
      complete = testCase.isComplete();
      }

    else
      {
      // No, bind next variable.
      VarDef nextVar = vars[ start];

      // Next variable already bound?
      if( testCase.getValue( nextVar) != null)
        {
        // Yes, complete remaining variables.
        complete = completeBindings( testCase, tuples, vars, start + 1);
        }

      else
        {
        // No, look for a compatible tuple to add that will bind the next variable,
        // preferably one not yet used.
        logger_.debug( "{}: Adding binding for var={}", this, nextVar);
        
        Iterator<Tuple> varTuples =
          IteratorUtils.chainedIterator
          ( tuples.getUnused( nextVar),
            
            IteratorUtils.chainedIterator
            ( tuples.getUsed( nextVar, false),
              
              IteratorUtils.chainedIterator
              ( tuples.getUsed( nextVar, true),
                getNA( nextVar))));
        
        Tuple tuple;
        Tuple tupleAdded;
        for( tuple = null,
               tupleAdded = null;

             // More tuples to try?
             varTuples.hasNext()
               && !( // Compatible tuple found?
                    (tupleAdded = testCase.addCompatible( (tuple = varTuples.next()))) != null

                    // Can we complete bindings for remaining variables?
                    && (complete = completeBindings( testCase, tuples, vars, start + 1)));
             
             tupleAdded = null)
          {
          if( tupleAdded != null)
            {
            logger_.debug( "{}: removing tuple={}", this, tupleAdded);
            testCase.removeBindings( tupleAdded);
            }
          }

        if( complete)
          {
          // Test case is complete -- mark any tuple added used.
          tuples.used( tuple);
          }
        }
      }
    
    return complete;
    }

  /**
   * Returns an iterator that contains the "not applicable" binding for the given variable. Returns an empty list if this
   * variable is not {@link VarDef#isOptional optional}.
   */
  private Iterator<Tuple> getNA( VarDef var)
    {
    List<Tuple> na = new ArrayList<Tuple>(1);
    if( var.isOptional())
      {
      na.add( new Tuple( new VarBindingDef( var, VarValueDef.NA)));
      }

    return na.iterator();
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
            }),
        VarDef.class);
    }

  /**
   * Returns the all valid input tuples required for generated test cases.
   */
  private VarTupleSet getValidTupleSet( RandSeq randSeq, FunctionInputDef inputDef)
    {
    List<Tuple> validTuples = new ArrayList<Tuple>();

    // Get tuple sets required for each specified combiner,
    // ordered for "greedy" processing, i.e. biggest tuples first.
    final TupleCombiner[] combiners = new TupleCombiner[ getCombiners().size()];
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

    // For all input variables that do not belong to a combiner tuple set...
    List<VarDef> uncombinedVars =
      IteratorUtils.toList
      ( IteratorUtils.filteredIterator
        ( new VarDefIterator( inputDef),
          new Predicate<VarDef>()
            {
            public boolean evaluate( VarDef var)
              {
              int i;
              for( i = 0; i < combiners.length && !combiners[i].isEligible( var); i++);
              return i >= combiners.length;
              }
            }));

    if( !uncombinedVars.isEmpty())
      {
      // ... add the default tuples.
      validTuples.addAll
        ( RandSeq.order
          ( randSeq,
            TupleCombiner.getTuples
            ( uncombinedVars,
              Math.min( uncombinedVars.size(), getDefaultTupleSize()))));
      }
    
    return new VarTupleSet( validTuples);
    }

  /**
   * Returns a set of failure {@link TestCaseDef test case definitions} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  private List<TestCaseDef> getFailureCases( RandSeq randSeq, FunctionInputDef inputDef, FunctionTestDef baseTests, List<TestCaseDef> validCases)
    {
    logger_.debug( "{}: creating failure test cases", inputDef);
    
    List<TestCaseDef> failureCases = new ArrayList<TestCaseDef>();
    VarTupleSet tuples = getFailureTupleSet( randSeq, inputDef);

    // For each failure input tuple not yet used in a test case...
    int validStart;
    int validUsed;
    int validCount;
    Tuple nextUnused;
    for( validCount = validCases.size(),
           validStart = 0;
         
         (nextUnused = tuples.getNextUnused()) != null;

         validStart = (validUsed + 1) % validCount)
      {
      // Create a new failure test case for this tuple, substituting the failure value into a compatible
      // valid case. To increase variability (and thus potential coverage), rotate the point where we
      // begin searching for a compatible valid case.
      logger_.debug( "Creating test case for tuple={}", nextUnused);
      TestCaseDef failureCase;
      int i;
      for( i = 0,
             validUsed = validStart,
             failureCase = null;
           
           i < validCount
             && (failureCase = createFailureCase( validCases.get( validUsed), nextUnused)) == null;

           i++,
             validUsed = (validUsed + 1) % validCount);

      if( failureCase == null)
        {
        throw new RuntimeException( "Can't create test case for tuple=" + nextUnused);
        }

      logger_.debug( "Completed test case={}", failureCase);
      tuples.used( nextUnused);
      failureCases.add( failureCase);
      }
    
    logger_.info( "{}: created {} failure test cases", inputDef, failureCases.size());
    return failureCases;
    }

  /**
   * Returns a new test case using the same variable bindings as the given valid case,
   * except for the substitution of the given failure binding. Returns null if
   * the <CODE>failureTuple</CODE> is not compatible with this valid case.
   */
  private TestCaseDef createFailureCase( TestCaseDef validCase, Tuple failureTuple)
    {
    TestCaseDef failureCase;

    try
      {
      failureCase = new TestCaseDef( validCase);
      failureCase.removeBindings( failureTuple);
      failureCase.addBindings( failureTuple);
      if( !failureCase.isComplete())
        {
        failureCase = null;
        }
      }
    catch( BindingException be)
      {
      logger_.debug( "{}, can't add tuple={}: {}", new Object[]{ validCase, failureTuple, be.getMessage()});
      failureCase = null;
      }
    catch( Exception e)
      {
      throw new RuntimeException( String.valueOf( validCase) + ", can't add tuple=", e);
      }

    return failureCase;
    }

  /**
   * Returns the all failure input tuples required for generated test cases.
   */
  private VarTupleSet getFailureTupleSet( RandSeq randSeq, FunctionInputDef inputDef)
    {
    List<Tuple> failureTuples = new ArrayList<Tuple>();

    for( VarDefIterator vars = new VarDefIterator( inputDef); vars.hasNext(); )
      {
      VarDef var = vars.next();
      for( Iterator<VarValueDef> failures = var.getFailureValues(); failures.hasNext(); )
        {
        failureTuples.add( new Tuple( new VarBindingDef( var, failures.next())));
        }
      }
    
    return new VarTupleSet( RandSeq.reorderIf( randSeq, failureTuples));
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
  private List<TupleCombiner> combiners_;

  private static final Logger logger_ = LoggerFactory.getLogger( TupleGenerator.class);
  }

