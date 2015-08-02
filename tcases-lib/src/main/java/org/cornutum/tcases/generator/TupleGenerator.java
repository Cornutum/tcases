//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.Cloneable;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.clonedList;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;
import org.apache.commons.lang3.ObjectUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * Generates {@link TestCase test cases} for a {@link FunctionInputDef function} that use
 * all specified N-tuples of valid variable values.
 *
 */
public class TupleGenerator implements ITestCaseGenerator, Cloneable<TupleGenerator>
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
      logger_.info( "{}: Generating test cases", inputDef);

      List<TestCaseDef> baseCases = getBaseCases( inputDef, baseTests);
      RandSeq randSeq = getRandomSeed()==null? null : new RandSeq( getRandomSeed());

      // Get all valid cases.
      VarTupleSet validTuples = getValidTupleSet( randSeq, inputDef);
      List<TestCaseDef> validCases = getBaseValidCases( inputDef, validTuples, baseCases);
      validCases.addAll( getValidCases( inputDef, validTuples));

      // Get all failure cases.
      VarTupleSet failureTuples = getFailureTupleSet( randSeq, inputDef);
      List<TestCaseDef> failureCases = getBaseFailureCases( inputDef, validTuples, failureTuples, baseCases);
      failureCases.addAll( getFailureCases( inputDef, failureTuples, validTuples));

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
    
      logger_.info( "{}: Completed {} test cases", inputDef, testCaseDefs.size());
      return testDef;
      }
    catch( Exception e)
      {
      logger_.error( String.valueOf( inputDef) + ": Can't create test cases", e);
      throw new RuntimeException( String.valueOf( inputDef) + ": Can't create test cases", e);
      }
    }

  /**
   * Returns a set of (possibly incomplete) {@link TestCaseDef test case definitions} corresponding the
   * the given base test cases. Variable bindings in the base tests that are no longer defined or
   * that have incompatible properties are ignored.
   */
  private List<TestCaseDef> getBaseCases( FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    logger_.debug( "{}: Creating base test cases", inputDef);

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
        logger_.debug( "Adding base test={}", baseTest);

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
            logger_.trace( "Var={} undefined", binding.getVar());
            }
          // Is value still defined?
          else if( value == null)
            {
            logger_.trace( "Value={} undefined", binding);
            }
          // Value type unchanged?
          else if( value.isValid() != binding.isValueValid())
            {
            // No, can't preserve this base test.
            logger_.debug
              ( "Can't add {}, {} changed to failure={}",
                new Object[]{ baseTest, binding, !value.isValid()});
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
    
    logger_.debug( "{}: Completed {} base test cases", inputDef, testCases.size());
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
      if( completeBindings( testCase, validTuples, getVarsRemaining( inputDef, testCase)))
        {
        logger_.debug( "Completed test case={}", testCase);
        validTuples.used( testCase);
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
    logger_.debug( "{}: Extending valid base test cases", inputDef);

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

    logger_.info( "{}: Extended {} valid base test cases", inputDef, testCases.size());
    return testCases;
   } 

  /**
   * Returns a set of failure {@link TestCaseDef test case definitions} that extend the given base test cases.
   */
  private List<TestCaseDef> getBaseFailureCases( FunctionInputDef inputDef, VarTupleSet validTuples, VarTupleSet failureTuples, List<TestCaseDef> baseCases)
    {
    logger_.debug( "{}: Extending base failure test cases", inputDef);

    Iterator<TestCaseDef> failureBaseCases =
      IteratorUtils.filteredIterator
      ( baseCases.iterator(),
        new Predicate<TestCaseDef>()
          {
          public boolean evaluate( TestCaseDef testCase)
            {
            return testCase.getInvalidVar() != null;
            }
          });

    List<TestCaseDef> testCases = extendBaseCases( inputDef, validTuples, failureBaseCases);

    // Consume all failure values used.
    for( TestCaseDef testCase : testCases)
      {
      VarDef failureVar = testCase.getInvalidVar();
      failureTuples.used( new Tuple( new VarBindingDef( failureVar, testCase.getValue( failureVar))));
      }

    logger_.info( "{}: Extended {} base failure test cases", inputDef, testCases.size());
    return testCases;
   }

  /**
   * Returns a set of valid {@link TestCaseDef test case definitions} for the given function input definition.
   */
  private List<TestCaseDef> getValidCases( FunctionInputDef inputDef, VarTupleSet validTuples)
    {
    logger_.debug( "{}: Creating valid test cases", inputDef);
    
    List<TestCaseDef> validCases = new ArrayList<TestCaseDef>();

    // For each valid input tuple not yet used in a test case...
    Tuple nextUnused;
    while( (nextUnused = validTuples.getNextUnused()) != null)
      {
      // Completed bindings for all variables?
      TestCaseDef validCase = createTestCase( nextUnused, inputDef, validTuples);
      if( validCase != null)
        {
        // Yes, add new valid test case.
        validTuples.used( validCase);
        validCases.add( validCase);
        }

      else
        {
        // No, remove infeasible tuple.
        validTuples.remove( nextUnused);
        }
      }

    logger_.info( "{}: Created {} valid test cases", inputDef, validCases.size());
    return validCases;
    }

  /**
   * Creates a new {@link TestCaseDef test case} uses the given tuple. 
   */
  private TestCaseDef createTestCase( Tuple tuple, FunctionInputDef inputDef, VarTupleSet validTuples)
    {
    logger_.debug( "Creating new test case for tuple={}", tuple);

    // Create a new test case for this tuple.
    TestCaseDef newCase = new TestCaseDef();
    try
      {
      newCase.addBindings( tuple);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't initialize new test case", e);
      }

    // Completed bindings for remaining variables?
    if( completeBindings( newCase, validTuples, getVarsRemaining( inputDef, newCase)))
      {
      // Yes, return new test case.
      logger_.debug( "Completed test case={}", newCase);
      }

    // Is this an infeasible tuple?
    else if( tuple.size() > 1)
      {
      // Yes, log a warning.
      logger_.warn( "Can't create test case for tuple={}", tuple);
      newCase = null;
      }

    else
      {
      // An infeasible single value is an input definition error.
      throw new RuntimeException( "Can't create test case for tuple=" + tuple);
      }

    return newCase;
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables,
   * starting with the given index. Returns true if all variables have been bound.
   */
  private boolean completeBindings( TestCaseDef testCase, VarTupleSet tuples, List<VarDef> vars)
    {
    boolean complete;

    // All variables bound?
    if( vars.isEmpty())
      {
      // Yes, all conditions satisified?
      complete = testCase.isSatisfied();
      }

    else
      {
      // No, look for a compatible tuple to add that will bind more variables.    
      complete = false;
    
      List<VarDef> reqVars = getSatisfyingVars( testCase, vars);
      logger_.trace
        ( "Complete bindings for vars={}, required={}, reqVars={}",
          new Object[]{ vars, testCase.getRequired(), reqVars});

      // Continue only if some unbound vars can satisfy the current test case conditions.
      if( !reqVars.isEmpty())
        {
        // Evaluate all potentially-satisfying tuples.
        Iterator<Tuple> satisfyingTuples =
          IteratorUtils.chainedIterator
          ( getSatisfyingTuples( testCase, tuples.getUnused( reqVars)),
            
            IteratorUtils.chainedIterator
            ( getSatisfyingTuples( testCase, tuples.getUsed( reqVars, false)),

              getSatisfyingTuples( testCase, tuples.getUsed( reqVars, true))));

        if( testCase.isSatisfied())
          {
          // If all current test case conditions satisfied, allow NA bindings for optional variables.
          satisfyingTuples =
            IteratorUtils.chainedIterator
            ( satisfyingTuples,
              getNA( vars));
          }
        
        Tuple tuple;
        Tuple tupleAdded;
        for( complete = false,
               tuple = null,
               tupleAdded = null;

             // More tuples to try?
             satisfyingTuples.hasNext()
               && !( // Compatible tuple found?
                     (tupleAdded = testCase.addCompatible( (tuple = satisfyingTuples.next()))) != null

                     // Did this tuple create an feasible combination?
                     && !testCase.isInfeasible()

                     // Can we complete bindings for remaining variables?
                     && (complete = completeBindings( testCase, tuples, getVarsRemaining( vars, tupleAdded))));
             
             tupleAdded = null)
          {
          if( tupleAdded != null)
            {
            logger_.debug( "Removing tuple={}", tupleAdded);
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
   * Returns the members of the given list of variables that could partially satisfy conditions for the given test case.
   */
  private List<VarDef> getSatisfyingVars( TestCaseDef testCase, List<VarDef> vars)
    {
    return IteratorUtils.toList( IteratorUtils.filteredIterator( vars.iterator(), testCase.getVarSatisfies()));
    }

  /**
   * Returns an iterator for the members of the given list of tuples that could partially satisfy conditions for the given test case.
   */
  private Iterator<Tuple> getSatisfyingTuples( TestCaseDef testCase, Iterator<Tuple> tuples)
    {
    return
      testCase.isSatisfied()
      ? tuples
      : IteratorUtils.filteredIterator( tuples, testCase.getTupleSatisfies());
    }

  /**
   * Returns an iterator that contains the "not applicable" binding for any of the given variables
   * that is {@link VarDef#isOptional optional}.
   */
  private Iterator<Tuple> getNA( List<VarDef> vars)
    {
    List<Tuple> na = new ArrayList<Tuple>( vars.size());
    for( Iterator<VarDef> naVars = vars.iterator();
         naVars.hasNext();)
      {
      VarDef var = naVars.next();
      if( var.isOptional())
        {
        na.add( new Tuple( new VarBindingDef( var, VarValueDef.NA)));
        }
      }

    return na.iterator();
    }

  /**
   * Returns the set of input variables not yet bound by the given test case.
   */
  private List<VarDef> getVarsRemaining( FunctionInputDef inputDef, final TestCaseDef testCase)
    {
    return IteratorUtils.toList
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
   * Returns the members of the given set of input variables not bound by the given tuple.
   */
  private List<VarDef> getVarsRemaining( List<VarDef> vars, final Tuple tuple)
    {
    return
      IteratorUtils.toList
      ( IteratorUtils.filteredIterator
        ( vars.iterator(),
          new Predicate<VarDef>()
            {
            public boolean evaluate( VarDef varDef)
              {
              return tuple.getBinding( varDef) == null;
              }
            }
          ));
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
            getUncombinedTuples
            ( uncombinedVars,
              Math.min
              ( varCount,
                defaultTupleSize < 1? varCount : defaultTupleSize))));
      }
    
    return new VarTupleSet( validTuples);
    }

  /**
   * Returns default tuples for all uncombined variables.
   */
  private Collection<Tuple> getUncombinedTuples( List<VarDef> uncombinedVars, int defaultTupleSize)
    {
    Collection<Tuple> tuples = TupleCombiner.getTuples( uncombinedVars, defaultTupleSize);
    if( defaultTupleSize == 1)
      {
      for( Tuple tuple : tuples)
        {
        VarValueDef value = tuple.getBindings().next().getValueDef();
        tuple.setOnce( value.getType() == VarValueDef.Type.ONCE);
        }
      }
    return tuples;
    }

  /**
   * Returns a set of failure {@link TestCaseDef test case definitions} for the given function input definition.
   */
  private List<TestCaseDef> getFailureCases( FunctionInputDef inputDef, VarTupleSet failureTuples, VarTupleSet validTuples)
    {
    logger_.debug( "{}: Creating failure test cases", inputDef);
    
    List<TestCaseDef> failureCases = new ArrayList<TestCaseDef>();

    // For each failure input tuple not yet used in a test case...
    Tuple nextUnused;
    while( (nextUnused = failureTuples.getNextUnused()) != null)
      {
      // Completed bindings for all variables?
      TestCaseDef failureCase = createTestCase( nextUnused, inputDef, validTuples);
      if( failureCase != null)
        {
        // Yes, add new failure test case.
        failureTuples.used( nextUnused);
        failureCases.add( failureCase);
        }
      }

    logger_.info( "{}: Created {} failure test cases", inputDef, failureCases.size());
    return failureCases;
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
  
  /**
   * Returns a copy of this object.
   */
  public TupleGenerator cloneOf()
    {
    TupleGenerator other = new TupleGenerator();
    other.setRandomSeed( getRandomSeed());
    other.setDefaultTupleSize( getDefaultTupleSize());
    other.setCombiners( clonedList( getCombiners()));
    return other;
    }

  @SuppressWarnings("deprecation")
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

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( getRandomSeed())
      ^ getDefaultTupleSize()
      ^ getCombiners().hashCode();
    }
  
  private Long        seed_;
  private int         defaultTupleSize_;
  private List<TupleCombiner> combiners_;

  private static final Logger logger_ = LoggerFactory.getLogger( TupleGenerator.class);
  }

