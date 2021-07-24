//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.CartesianProduct;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.MultiMapUtils;
import org.apache.commons.collections4.MultiValuedMap;
import static org.apache.commons.collections4.functors.NOPTransformer.nopTransformer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import static java.util.stream.Collectors.toList;

/**
 * Generates {@link TestCase test cases} for a {@link FunctionInputDef function} that use
 * all specified N-tuples of valid variable values.
 *
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
  @Override
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
  @Override
  public FunctionTestDef getTests( FunctionInputDef inputDef, FunctionTestDef baseTests)
    {
    try
      {
      logger_.info( "{}: Preparing constraint info", inputDef);
      createPropertyProviders( inputDef);
      
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
        testCase.setName( baseTest.getName());
        
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
      if( makeComplete( testCase, validTuples, getVarsRemaining( inputDef, testCase)))
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
      IteratorUtils.filteredIterator(
        baseCases.iterator(),
        testCase -> testCase.getInvalidVar() == null);

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
      IteratorUtils.filteredIterator(
        baseCases.iterator(),
        testCase -> testCase.getInvalidVar() != null);

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
      newCase.setName( tuple);
      newCase.addBindings( tuple);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't initialize new test case", e);
      }

    // Completed bindings for remaining variables?
    if( makeComplete( newCase, validTuples, getVarsRemaining( inputDef, newCase)))
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
   * Using selections from the given set of tuples, completes binding for all remaining variables.
   * Returns true if all variables have been bound.
   */
  private boolean makeComplete( TestCaseDef testCase, VarTupleSet tuples, List<VarDef> vars)
    {
    boolean complete;

    // Test case still missing a required property?
    if( testCase.isSatisfied())
      {
      // No, complete bindings for remaining variables
      complete = completeSatisfied( testCase, tuples, vars);
      }
    else
      {
      // Yes, find tuples that contain satisfying bindings.
      int prevBindings = testCase.getBindingCount();
      Iterator<Tuple> satisfyingTuples = getSatisfyingTuples( testCase, tuples);
      for( complete = false;

           satisfyingTuples.hasNext()
             && !(
               // Does this tuple lead to satisfaction of all current test case conditions?
               makeSatisfied( testCase, tuples, satisfyingTuples.next())

               // Can we complete bindings for remaining variables?
               && (complete = completeSatisfied( testCase, tuples, vars)));

           // No, try next tuple
           testCase.revertBindings( prevBindings));
      }

    return complete;
    }

  /**
   * Using selections from the given set of tuples, completes binding for all remaining variables.
   * Returns true if all variables have been bound.
   */
  private boolean completeSatisfied( TestCaseDef testCase, VarTupleSet tuples, List<VarDef> vars)
    {
    List<VarDef> varsRemaining = getVarsRemaining( vars, testCase);
    VarDef varApplicable;
    boolean complete; 

    // Any variables remaining unbound?
    if( varsRemaining.isEmpty())
      {
      // No, this test case is complete.
      complete = true;
      }
    
    // Any variables remaining that are currently applicable to this test case?
    else if( (varApplicable = varsRemaining.stream().filter( v -> testCase.isApplicable(v)).findFirst().orElse( null)) == null)
      {
      // No, continue with an NA binding for the next variable
      testCase.addCompatible( getNaBindingFor( varsRemaining.get( 0)));
      complete = makeComplete( testCase, tuples, varsRemaining);
      }

    else
      {
      // Find an applicable binding that will lead to a complete test case
      int prevBindings = testCase.getBindingCount();
      Iterator<Tuple> bindingTuples = getBindingsFor( tuples, varApplicable);
      for( complete = false;

           // More tuples to try?
           bindingTuples.hasNext()
             && !(
               // Compatible tuple found?
               testCase.addCompatible( bindingTuples.next()) != null

               // Did this tuple create an infeasible combination?
               && !testCase.isInfeasible()

               // Can we complete bindings for remaining variables?
               && (complete = makeComplete( testCase, tuples, varsRemaining)));
             
           // No path to completion with this tuple -- try the next one.
           testCase.revertBindings( prevBindings));
      }
    
    return complete;
    }

  /**
   * Using selections from the given set of tuples, completes bindings to satisfy all current test case conditions.
   * Returns true if and only if all conditions satisfied.
   */
  private boolean makeSatisfied( TestCaseDef testCase, VarTupleSet tuples)
    {
    // Test case still missing a required property?
    boolean satisfied = testCase.isSatisfied();
    if( !satisfied)
      {
      // Yes, find tuples that contain satisfying bindings.
      int prevBindings = testCase.getBindingCount();
      for( Iterator<Tuple> satisfyingTuples = getSatisfyingTuples( testCase, tuples);

           // Does this tuple lead to satisfaction of all current test case conditions?
           satisfyingTuples.hasNext()
             && !(satisfied = makeSatisfied( testCase, tuples, satisfyingTuples.next()));

           // No, try next tuple
           testCase.revertBindings( prevBindings));
      }

    return satisfied;
    }

  /**
   * Starting with the given tuple, completes bindings to satisfy all current test case conditions,
   * using additional selections from the given set of tuples if necessary.
   * Returns true if and only if all conditions satisfied.
   */
  private boolean makeSatisfied( TestCaseDef testCase, VarTupleSet tuples, Tuple satisfyingTuple)
    {
    return
      // Compatible tuple found?
      testCase.addCompatible( satisfyingTuple) != null

      // Did this tuple create an infeasible combination?
      && !testCase.isInfeasible()

      // Can also we satisfy any new conditions?
      && makeSatisfied( testCase, tuples);
    }

  /**
   * Returns tuples that contain a binding for the given variable.
   */
  private Iterator<Tuple> getBindingsFor( VarTupleSet tuples, VarDef var)
    {
    return
      IteratorUtils.chainedIterator(
        tuples.getUnused( var),
        IteratorUtils.chainedIterator(
          tuples.getUsed( var),
          IteratorUtils.chainedIterator(
            tuples.getUsedOnce( var),
            getOneTuples( var))));
    }

  /**
   * Returns a single NA binding for the given variable.
   */
  private Tuple getNaBindingFor( VarDef var)
    {
    return new Tuple( new VarBindingDef( var, VarNaDef.NA));
    }

  /**
   * Returns all valid 1-tuples for the given variable.
   */
  private Iterator<Tuple> getOneTuples( VarDef var)
    {
    return
      IteratorUtils.transformedIterator(
        var.getValidValues(),
        value -> Tuple.of( new VarBindingDef( var, value)));
    }

  /**
   * Returns the set of tuples that could satisfy conditions required by the given test case.
   */
  private Iterator<Tuple> getSatisfyingTuples( final TestCaseDef testCase, VarTupleSet varTupleSet)
    {
    final Comparator<VarBindingDef> byUsage = byUsage( varTupleSet);

    return
      IteratorUtils.transformedIterator(
        // Iterate over all combinations of bindings...
        new CartesianProduct<VarBindingDef>(

          // ...combining members from all sets of bindings...
          new ArrayList<Set<VarBindingDef>>(
            CollectionUtils.collect(
              // ...where each set of bindings is derived from a disjunct of unsatisfied test case conditions....
              testCase.getRequired().getDisjuncts(),

              // ...and contains the set of compatible bindings that could satisfy this disjunct...
              disjunct ->
                {
                Set<String> unsatisfied =
                  CollectionUtils.collect(
                    disjunct.getAssertions(),
                    assertion -> assertion.getProperty(),
                    new HashSet<String>());

                Iterator<VarBindingDef> satisfyingBindings =
                  IteratorUtils.filteredIterator(
                    getPropertyProviders( unsatisfied).iterator(),
                    binding -> testCase.isCompatible( binding));

                // ...arranging satisfying bindings in order of decreasing preference...
                return
                  CollectionUtils.collect(
                    satisfyingBindings,
                    nopTransformer(),
                    new TreeSet<VarBindingDef>( byUsage));
                },

              // ...arranging sets in a well-defined order for repeatable combinations...
              new TreeSet<Set<VarBindingDef>>( varBindingSetSorter_))),
          
          // ...ignoring any infeasible combinations...
          bindings -> Tuple.of( bindings) != null),
        
         // ... forming each combination of satisfying bindings into a tuple...
        Tuple::of);
    }

  /**
   * Returns the set of input variables not yet bound by the given test case.
   */
  private List<VarDef> getVarsRemaining( FunctionInputDef inputDef, TestCaseDef testCase)
    {
    return getVarsRemaining( new VarDefIterator( inputDef), testCase);
    }

  /**
   * Returns the members of the given set of input variables not bound by the given test case.
   */
  private List<VarDef> getVarsRemaining( List<VarDef> vars, TestCaseDef testCase)
    {
    return getVarsRemaining( vars.iterator(), testCase);
    }

  /**
   * Returns the members of the given set of input variables not bound by the given test case.
   */
  private List<VarDef> getVarsRemaining( Iterator<VarDef> vars, final TestCaseDef testCase)
    {
    return
      IteratorUtils.toList(
        IteratorUtils.filteredIterator(
          vars,
          var -> testCase.getBinding( var) == null));
    }

  /**
   * Returns the all valid input tuples required for generated test cases.
   */
  private VarTupleSet getValidTupleSet( RandSeq randSeq, FunctionInputDef inputDef)
    {
    List<Tuple> validTuples = new ArrayList<Tuple>();

    // Get tuple sets required for each specified combiner, ordered for "greedy" processing, i.e. biggest tuples first.
    // For this purpose, "all permutations" is considered the maximum tuple size, even though in practice it might not be.
    getCombiners()
      .stream()
      .sorted( byTupleSize_)
      .forEach( combiner -> validTuples.addAll( RandSeq.order( randSeq, combiner.getTuples( inputDef))));

    // For all input variables that do not belong to a combiner tuple set...
    List<VarDef> uncombinedVars =
      IteratorUtils.toList(
        IteratorUtils.filteredIterator(
          new VarDefIterator( inputDef),
          this::isUncombined));

    if( !uncombinedVars.isEmpty())
      {
      // ... add the default tuples.
      int defaultTupleSize = getDefaultTupleSize();
      int varCount = uncombinedVars.size();
      validTuples.addAll(
        RandSeq.order(
          randSeq,
          getUncombinedTuples(
            uncombinedVars,
            Math.min(
              varCount,
              defaultTupleSize < 1? varCount : defaultTupleSize))));
      }
    
    return new VarTupleSet( validTuples);
    }

  /**
   * Returns if the given variable does not belong to any combiner tuple set.
   */
  private boolean isUncombined( VarDef var)
    {
    return combiners_.stream().noneMatch( combiner -> combiner.isEligible( var));
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

  /**
   * Return a map that associates each value property with the set of bindings that provide it.
   */
  private MultiValuedMap<String,VarBindingDef> createPropertyProviders( FunctionInputDef inputDef)
    {
    propertyProviders_ = MultiMapUtils.newListValuedHashMap();
    for( VarDefIterator varDefs = new VarDefIterator( inputDef.getVarDefs()); varDefs.hasNext(); )
      {
      VarDef varDef = varDefs.next();
      for( Iterator<VarValueDef> values = varDef.getValidValues(); values.hasNext(); )
        {
        VarValueDef value = values.next();
        if( value.hasProperties())
          {
          VarBindingDef binding = new VarBindingDef( varDef, value);
          for( Iterator<String> properties = value.getProperties().iterator(); properties.hasNext(); )
            {
            propertyProviders_.put( properties.next(), binding);
            }
          }
        }
      }

    return propertyProviders_;
    }

  /**
   * Returns the set of bindings that provide at least one of the given properties
   */
  private Set<VarBindingDef> getPropertyProviders( Set<String> properties)
    {
    Set<VarBindingDef> bindings = new HashSet<VarBindingDef>();
    for( String property : properties)
      {
      bindings.addAll( propertyProviders_.get( property));
      }

    return bindings;
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "defaultTuples", getDefaultTupleSize())
      .append( "seed", getRandomSeed())
      .toString();
    }

  /**
   * Returns a comparator that orders bindings by decreasing preference, preferring bindings that are
   * less used.
   */
  private Comparator<VarBindingDef> byUsage( final VarTupleSet varTupleSet)
    {
    return
      new Comparator<VarBindingDef>()
        {
        @Override
        public int compare( VarBindingDef binding1, VarBindingDef binding2)
          {
          // Compare by usage score: higher score is preferred.
          int resultScore = getScore( binding2).compareTo( getScore( binding1));
          return
            // If equal usage score...
            resultScore == 0
            // ...then compare lexigraphically
            ? varBindingDefSorter_.compare( binding1, binding2)
            : resultScore;
          }

        private Integer getScore( VarBindingDef binding)
          {
          Integer score = bindingScores_.get( binding);
          if( score == null)
            {
            // Preferred higher "unused-ness" and lower "used-ness" (especially among once-only tuples)
            int maxScore = 1000;
            int unusedScore = (int) (varTupleSet.getUnusedScore( binding) * maxScore);
            int usedScore = (int) ((1.0 - varTupleSet.getUsedScore( binding)) * (maxScore - 1));
            int usedOnceScore = (int) ((1.0 - varTupleSet.getUsedOnceScore( binding)) * (maxScore - 1));
            score = ((unusedScore * maxScore) + usedOnceScore) * maxScore + usedScore;
            bindingScores_.put( binding, score);
            }
          
          return score;
          }
        private Map<VarBindingDef,Integer> bindingScores_ = new HashMap<VarBindingDef,Integer>();
      };
    }
  
  /**
   * Returns a copy of this object.
   */
  @Override
  public ITestCaseGenerator cloneOf()
    {
    TupleGenerator other = new TupleGenerator();
    other.setRandomSeed( getRandomSeed());
    other.setDefaultTupleSize( getDefaultTupleSize());
    other.setCombiners( getCombiners().stream().map( TupleCombiner::cloneOf).collect( toList()));
    return other;
    }

  @Override
  public boolean equals( Object object)
    {
    TupleGenerator other =
      object != null && object.getClass().equals( getClass())
      ? (TupleGenerator) object
      : null;

    return
      other != null
      && Objects.equals( other.getRandomSeed(), getRandomSeed())
      && other.getDefaultTupleSize() == getDefaultTupleSize()
      && other.getCombiners().equals( getCombiners()); 
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getRandomSeed())
      ^ getDefaultTupleSize()
      ^ getCombiners().hashCode();
    }
  
  private Long seed_;
  private int defaultTupleSize_;
  private List<TupleCombiner> combiners_;
  private MultiValuedMap<String,VarBindingDef> propertyProviders_;

  private static final Logger logger_ = LoggerFactory.getLogger( TupleGenerator.class);

  private static final Comparator<VarBindingDef> varBindingDefSorter_ =
    new Comparator<VarBindingDef>()
      {
      @Override
    public int compare( VarBindingDef binding1, VarBindingDef binding2)
        {
        String var1 = binding1.getVarDef().getPathName();
        String var2 = binding2.getVarDef().getPathName();
        int result = var1.compareTo( var2);
        if( result == 0)
          {
          String value1 = String.valueOf( binding1.getValueDef().getName());
          String value2 = String.valueOf( binding2.getValueDef().getName());
          result = value1.compareTo( value2);
          }
          
        return result;
        }
      };

  private static final Comparator<Set<VarBindingDef>> varBindingSetSorter_ =
    new Comparator<Set<VarBindingDef>>()
      {
      @Override
    public int compare( Set<VarBindingDef> bindingSet1, Set<VarBindingDef> bindingSet2)
        {
        int result = bindingSet1.size() - bindingSet2.size();
        if( result == 0)
          {
          Iterator<VarBindingDef> bindings1; 
          Iterator<VarBindingDef> bindings2; 
          for( bindings1 = bindingSet1.iterator(),
                 bindings2 = bindingSet2.iterator();

               bindings1.hasNext()
                 && (result = varBindingDefSorter_.compare( bindings1.next(), bindings2.next())) == 0;
               );
          }
          
        return result;
        }
      };

  private static final Comparator<TupleCombiner> byTupleSize_ =
    new Comparator<TupleCombiner>()
      {
      @Override
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
      };
  }
