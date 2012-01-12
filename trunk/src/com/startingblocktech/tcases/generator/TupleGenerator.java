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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

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

      List<TestCaseDef> baseCases = getBaseCases( inputDef, baseTests);
      RandSeq randSeq = getRandomSeed()==null? null : new RandSeq( getRandomSeed());

      // Get all valid cases.
      VarTupleSet validTuples = getValidTupleSet( randSeq, inputDef);
      List<TestCaseDef> validCases = getBaseValidCases( inputDef, validTuples, baseCases);
      validCases.addAll( getValidCases( inputDef, validTuples));

      // Get all failure cases.
      VarTupleSet failureTuples = getFailureTupleSet( randSeq, inputDef);
      List<TestCaseDef> failureCases = getBaseFailureCases( inputDef, validTuples, failureTuples, baseCases);
      failureCases.addAll( getFailureCases( inputDef, failureTuples, validCases));

      FunctionTestDef testDef = new FunctionTestDef( inputDef.getName());

      // Create test cases, in order of increasing id.
      List<TestCaseDef> testCaseDefs = new ArrayList<TestCaseDef>();
      testCaseDefs.addAll( validCases);
      testCaseDefs.addAll( failureCases);
      Collections.sort( testCaseDefs);

      int nextId = -1;
      for( TestCaseDef testCase : testCaseDefs)
        {
        Integer id = testCase.getId();
        nextId = id==null? nextId + 1 : id.intValue();
        testDef.addTestCase( testCase.createTestCase( nextId));
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
   * Returns a set of (possibly incomplete) {@link TestCaseDef test case definitions} corresponding the
   * the given base test cases. Variable bindings in the base tests that are no longer defined or
   * that have incompatible properties are ignored.
   */
  private List<TestCaseDef> getBaseCases( FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    logger_.debug( "{}: creating base test cases", inputDef);

    List<TestCaseDef> testCases = new ArrayList<TestCaseDef>();
    if( baseTests != null)
      {
      // For each base test case...
      for( Iterator<TestCase> baseCases = baseTests.getTestCases();
           baseCases.hasNext(); )
        {
        // Create the corresponding test case definition...
        TestCase baseTest = baseCases.next();
        TestCaseDef testCase = new TestCaseDef();
        testCase.setId( baseTest.getId());
        logger_.debug( "{}: adding base test={}", inputDef, baseTest);

        // For each variable binding...
        for( Iterator<VarBinding> bindings = baseTest.getVarBindings();
             testCase != null && bindings.hasNext();)
          {
          VarBinding binding = bindings.next();
          VarDef var = inputDef.findVarDefPath( binding.getVar());
          VarValueDef value = var==null? null : var.getValue( binding.getValue());

          // Is variable still defined?
          if( var == null)
            {
            logger_.debug( "{}: var={} undefined", inputDef, binding.getVar());
            }
          // Is value still defined?
          else if( value == null)
            {
            logger_.debug( "{}: value={} undefined", inputDef, binding);
            }
          // Value type unchanged?
          else if( value.isValid() != binding.isValueValid())
            {
            // No, can't preserve this base test.
            logger_.debug
              ( "{}: can't add {}, {} changed to failure={}",
                new Object[]{ inputDef, baseTest, binding, !value.isValid()});
            testCase = null;
            }
          else
            {
            // Add variable binding if still compatible.
            testCase.addCompatible( new Tuple( new VarBindingDef( var, value)));
            }
          }

        if( testCase != null && testCase.getVars().hasNext())
          {
          // Add new (possibly incomplete) test case definition for this base test.
          testCases.add( testCase);
          }
        }
      }
    
    logger_.debug( "{}: completed {} base test cases", inputDef, testCases.size());
    return testCases;
    }

  /**
   * Returns a set of valid {@link TestCaseDef test case definitions} that extend the given base test cases.
   */
  private List<TestCaseDef> extendBaseCases( FunctionInputDef inputDef, VarTupleSet validTuples, Iterator<TestCaseDef> baseCases)
    {
    List<TestCaseDef> testCases = new ArrayList<TestCaseDef>();
    
    // For each base test case...
    while( baseCases.hasNext())
      {
      TestCaseDef testCase = baseCases.next();
      logger_.debug( "Extending base test case={}", testCase);

      // Complete all variable bindings.
      Set<VarDef> unconsumed = new HashSet<VarDef>( IteratorUtils.toList( testCase.getVars()));
      if( completeBindings( testCase, validTuples, getBaseVarsRemaining( inputDef, testCase), 0, unconsumed))
        {
        logger_.debug( "Completed test case={}", testCase);
        testCases.add( testCase);
        }
      else
        {
        logger_.debug( "Can't complete test case={}", testCase);
        }
      }

    return testCases;
   }

  /**
   * Returns a set of valid {@link TestCaseDef test case definitions} that extend the given base test cases.
   */
  private List<TestCaseDef> getBaseValidCases( FunctionInputDef inputDef, VarTupleSet validTuples, List<TestCaseDef> baseCases)
    {
    logger_.debug( "{}: extending valid base test cases", inputDef);

    Iterator<TestCaseDef> validBaseCases =
      IteratorUtils.filteredIterator
      ( baseCases.iterator(),
        new Predicate<TestCaseDef>()
          {
          public boolean evaluate( TestCaseDef testCase)
            {
            return testCase.getInvalidVar() == null;
            }
          });

    List<TestCaseDef> testCases = extendBaseCases( inputDef, validTuples, validBaseCases);

    logger_.info( "{}: extended {} valid base test cases", inputDef, testCases.size());
    return testCases;
   } 

  /**
   * Returns a set of failure {@link TestCaseDef test case definitions} that extend the given base test cases.
   */
  private List<TestCaseDef> getBaseFailureCases( FunctionInputDef inputDef, VarTupleSet validTuples, VarTupleSet failureTuples, List<TestCaseDef> baseCases)
    {
    logger_.debug( "{}: extending base failure test cases", inputDef);

    Iterator<TestCaseDef> validBaseCases =
      IteratorUtils.filteredIterator
      ( baseCases.iterator(),
        new Predicate<TestCaseDef>()
          {
          public boolean evaluate( TestCaseDef testCase)
            {
            return testCase.getInvalidVar() != null;
            }
          });

    List<TestCaseDef> testCases = extendBaseCases( inputDef, validTuples, validBaseCases);

    // Consume all failure values used.
    for( TestCaseDef testCase : testCases)
      {
      VarDef failureVar = testCase.getInvalidVar();
      failureTuples.used( new Tuple( new VarBindingDef( failureVar, testCase.getValue( failureVar))));
      }

    logger_.info( "{}: extended {} base failure test cases", inputDef, testCases.size());
    return testCases;
   }

  /**
   * Returns a set of valid {@link TestCaseDef test case definitions} for the given function input definition.
   */
  private List<TestCaseDef> getValidCases( FunctionInputDef inputDef, VarTupleSet validTuples)
    {
    logger_.debug( "{}: creating valid test cases", inputDef);
    
    List<TestCaseDef> validCases = new ArrayList<TestCaseDef>();

    // For each valid input tuple not yet used in a test case...
    Tuple nextUnused;
    while( (nextUnused = validTuples.getNextUnused()) != null)
      {
      // Create a new test case for this tuple.
      logger_.debug( "Creating new test case for tuple={}", nextUnused);

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
      if( !completeBindings( validCase, validTuples, getVarsRemaining( inputDef, validCase), 0))
        {
        throw new RuntimeException( "Can't create test case for tuple=" + nextUnused);
        }
      
      logger_.debug( "Completed test case={}", validCase);
      validTuples.used( nextUnused);
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
    return completeBindings( testCase, tuples, vars, start, null);
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables,
   * starting with the given index. Returns true if all variables have been bound.
   */
  private boolean completeBindings( TestCaseDef testCase, VarTupleSet tuples, VarDef[] vars, int start, Set<VarDef> unconsumed)
    {
    VarDef nextVar =
      start >= vars.length
      ? null
      : vars[ start];

    return
      // All variables bound?
      nextVar == null?
      // Yes, all conditions satisified?
      testCase.isSatisfied() :

      // Next variable already bound?
      testCase.getValue( nextVar) == null?
      // No, look for a compatible tuple to add that will bind the next variable.
      completeForVarUnbound( testCase, tuples, vars, start, unconsumed) :

      // Tuple already consumed by this variable binding?
      unconsumed != null && unconsumed.contains( nextVar)?
      // No, look for unused tuple to consume with this variable binding.
      completeForVarUnconsumed( testCase, tuples, vars, start, unconsumed) :
      
      // Else nothing to do for this variable -- complete remaining variables.
      completeBindings( testCase, tuples, vars, start + 1, unconsumed);
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables,
   * starting with the given index. Returns true if all variables have been bound.
   */
  private boolean completeForVarUnbound( TestCaseDef testCase, VarTupleSet tuples, VarDef[] vars, int start, Set<VarDef> unconsumed)
    {
    // Look for a compatible tuple to add that will bind the next variable,
    // preferably one not yet used.
    VarDef nextVar = vars[ start];
    logger_.debug( "{}: Adding binding for var={}", this, nextVar);
        
    Iterator<Tuple> varTuples =
      IteratorUtils.chainedIterator
      ( tuples.getUnused( nextVar),
            
        IteratorUtils.chainedIterator
        ( tuples.getUsed( nextVar, false),
              
          IteratorUtils.chainedIterator
          ( tuples.getUsed( nextVar, true),
            getNA( nextVar))));
        
    boolean complete;
    Tuple tuple;
    Tuple tupleAdded;
    for( complete = false,
           tuple = null,
           tupleAdded = null;

         // More tuples to try?
         varTuples.hasNext()
           && !( // Compatible tuple found?
                (tupleAdded = testCase.addCompatible( (tuple = varTuples.next()))) != null

                // Can we complete bindings for remaining variables?
                && (complete = completeBindings( testCase, tuples, vars, start + 1, unconsumed)));
             
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

    return complete;
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables,
   * starting with the given index. Returns true if all variables have been bound.
   */
  private boolean completeForVarUnconsumed( TestCaseDef testCase, VarTupleSet tuples, VarDef[] vars, int start, Set<VarDef> unconsumed)
    {
    // Look for unused tuple consumed by next variable binding.
    VarDef nextVar = vars[ start];
    Tuple tuple;
    Iterator<Tuple> varTuples;
    for( varTuples= tuples.getUnused( nextVar),
           tuple = null;
             
         varTuples.hasNext()
           && testCase.addCompatible( (tuple = varTuples.next())) == null;

         tuple = null);

    // With updated unconsumed bindings...
    Set<VarDef> unconsumedNew = new HashSet<VarDef>( unconsumed);
    unconsumedNew.remove( nextVar);
    if( tuple != null)
      {
      for( Iterator<VarBindingDef> bindings = tuple.getBindings();
           bindings.hasNext();
           unconsumedNew.remove( bindings.next().getVarDef()));
      }

    // ...complete remaining variables....
    boolean complete = completeBindings( testCase, tuples, vars, start + 1, unconsumedNew);
    if( complete && tuple != null)
      {
      // Test case is complete -- mark any tuple consumed.
      tuples.used( tuple);
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
  private VarDef[] getVarsRemaining( FunctionInputDef inputDef, TestCaseDef testCase)
    {
    return IteratorUtils.toArray( getVarsUnbound( inputDef, testCase), VarDef.class);
    }

  /**
   * Returns the set of input variables to be bound by the given base test case.
   */
  private VarDef[] getBaseVarsRemaining( FunctionInputDef inputDef, TestCaseDef baseCase)
    {
    return
      IteratorUtils.toArray
      ( IteratorUtils.chainedIterator
        ( baseCase.getVars(),
          getVarsUnbound( inputDef, baseCase)),
        VarDef.class);
    }

  /**
   * Returns the set of input variables not yet bound by the given test case.
   */
  private Iterator<VarDef> getVarsUnbound( FunctionInputDef inputDef, final TestCaseDef testCase)
    {
    return
      IteratorUtils.filteredIterator
      ( new VarDefIterator( inputDef),
        new Predicate<VarDef>()
          {
          public boolean evaluate( VarDef var)
            {
            return testCase.getBinding( var) == null;
            }
          });
    }

  /**
   * Returns the all valid input tuples required for generated test cases.
   */
  private VarTupleSet getValidTupleSet( RandSeq randSeq, FunctionInputDef inputDef)
    {
    List<Tuple> validTuples = new ArrayList<Tuple>();

    // Get tuple sets required for each specified combiner,
    // ordered for "greedy" processing, i.e. biggest tuples first.
    // For this purpose, "all permutations" is considered the maximum tuple size,
    // even though in practice it might not be.
    final TupleCombiner[] combiners = new TupleCombiner[ getCombiners().size()];
    getCombiners().toArray( combiners);
    Arrays.sort
      ( combiners,
        new Comparator<TupleCombiner>()
          {
          public int compare( TupleCombiner combiner1, TupleCombiner combiner2)
            {
            return
              effectiveTupleSize( combiner2.getTupleSize())
              - effectiveTupleSize( combiner1.getTupleSize());
            }

          private int effectiveTupleSize( int tupleSize)
            {
            return tupleSize < 1? Integer.MAX_VALUE : tupleSize;
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
      int defaultTupleSize = getDefaultTupleSize();
      int varCount = uncombinedVars.size();
      validTuples.addAll
        ( RandSeq.order
          ( randSeq,
            TupleCombiner.getTuples
            ( uncombinedVars,
              Math.min
              ( varCount,
                defaultTupleSize < 1? varCount : defaultTupleSize))));
      }
    
    return new VarTupleSet( validTuples);
    }

  /**
   * Returns a set of failure {@link TestCaseDef test case definitions} for the given function input definition.
   */
  private List<TestCaseDef> getFailureCases( FunctionInputDef inputDef, VarTupleSet failureTuples, List<TestCaseDef> validCases)
    {
    logger_.debug( "{}: creating failure test cases", inputDef);
    
    List<TestCaseDef> failureCases = new ArrayList<TestCaseDef>();

    // For each failure input tuple not yet used in a test case...
    int validStart;
    int validUsed;
    int validCount;
    Tuple nextUnused;
    for( validCount = validCases.size(),
           validStart = 0;
         
         (nextUnused = failureTuples.getNextUnused()) != null;

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
      failureTuples.used( nextUnused);
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
      if( !failureCase.isSatisfied())
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

