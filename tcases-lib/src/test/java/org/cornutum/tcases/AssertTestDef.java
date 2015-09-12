//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import static org.cornutum.tcases.util.Asserts.*;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;
import org.apache.commons.collections4.Transformer;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Provides methods for verifying test cases definitions.
 */
public final class AssertTestDef
  {
  /**
   * Reports a failure if any TestCase fails to bind the variables defined by the FunctionInputDef.
   */
  public static void assertTestCasesComplete( FunctionInputDef inputDef, FunctionTestDef testDef)
    {
    List<String> vars =
      IteratorUtils.toList
      ( IteratorUtils.transformedIterator
        ( new VarDefIterator( inputDef),
          new Transformer<VarDef,String>()
            {
            public String transform( VarDef varDef)
              {
              return varDef.getPathName();
              }
            }));
    
    for( Iterator<TestCase> testCases = testDef.getTestCases(); testCases.hasNext(); )
      {
      TestCase testCase = testCases.next();
      assertSetEquals
        ( "Vars, testCase=" + testCase.getId(),
          vars,
          IteratorUtils.transformedIterator
          ( testCase.getVarBindings(),
            new Transformer<VarBinding,String>()
              {
              public String transform( VarBinding varBinding)
                {
                return varBinding.getVar();
                }
              }));
      }
    }

  /**
   * Returns the subset of the given set of tuples that are included in at least one test case
   * from the given test definition.
   */
  public static Collection<Tuple> getTuplesIncluded( Collection<Tuple> tuples, final FunctionTestDef testDef)
    {
    return getTuplesIncluded( tuples, testDef, true);
    }

  /**
   * If <CODE>included</CODE> is true, returns the subset of the given set of tuples that are included in at least one test case
   * from the given test definition.
   *
   * Otherwise, if <CODE>included</CODE> is false, returns the subset of the given set of tuples that are not included in any test case.
   */
  private static Collection<Tuple> getTuplesIncluded( Collection<Tuple> tuples, final FunctionTestDef testDef, final boolean included)
    {
    return
      IteratorUtils.toList
      ( IteratorUtils.filteredIterator
        ( tuples.iterator(),
          new Predicate<Tuple>()
            {
            public boolean evaluate( final Tuple tuple)
              {
              return
                IteratorUtils.filteredIterator
                ( testDef.getTestCases(),
                  new Predicate<TestCase>()
                  {
                  public boolean evaluate( TestCase testCase)
                    {
                    return testCaseIncludes( testCase, tuple);
                    }
                  })
                .hasNext() == included;
              }
            }));
      }

  /**
   * Returns true if the given test case contains all bindings in the given tuple.
   */
  private static boolean testCaseIncludes( TestCase testCase, Tuple tuple)
    {
    Iterator<VarBindingDef> bindings;
    boolean includes;
    for( bindings = tuple.getBindings(),
           includes = true;

         bindings.hasNext()
           && includes;
         )
      {
      VarBindingDef binding = bindings.next();
      includes = testCaseIncludes( testCase, new VarBinding( binding.getVarDef(), binding.getValueDef()));
      }

    return includes;
    }

  /**
   * Returns true if the given test case contains the given binding
   */
  private static boolean testCaseIncludes( TestCase testCase, VarBinding binding)
    {
    return binding.equals( testCase.getVarBinding( binding.getVar()));
    }

  /**
   * Returns the number of test cases that include the given binding.
   */
  private static int getTestCasesIncluding( FunctionTestDef testDef, final VarBinding binding)
    {
    return
      CollectionUtils.countMatches
      ( IteratorUtils.toList( testDef.getTestCases()),
        new Predicate<TestCase>()
          {
          public boolean evaluate( TestCase testCase)
            {
            return testCaseIncludes( testCase, binding);
            }
          });
    }

  /**
   * Returns the number of test cases that include the given tuple.
   */
  private static int getTestCasesIncluding( FunctionTestDef testDef, final Tuple tuple)
    {
    return
      CollectionUtils.countMatches
      ( IteratorUtils.toList( testDef.getTestCases()),
        new Predicate<TestCase>()
          {
          public boolean evaluate( TestCase testCase)
            {
            return testCaseIncludes( testCase, tuple);
            }
          });
    }

  /**
   * Reports a failure if the number of test cases including the given binding does not equal the expected value.
   */
  public static void assertIncluded( FunctionTestDef testDef, int expectedCount, String var, String value)
    {
    assertIncluded( testDef, expectedCount, var, value, true);
    }

  /**
   * Reports a failure if the number of test cases including the given binding does not equal the expected value.
   */
  public static void assertIncluded( FunctionTestDef testDef, int expectedCount, String var, String value, boolean isValid)
    {
    VarBinding binding = new VarBinding( var, value);
    binding.setValueValid( isValid);
    assertIncluded( testDef, expectedCount, binding);
    }

  /**
   * Reports a failure if the number of test cases including the given binding does not equal the expected value.
   */
  public static void assertIncluded( FunctionTestDef testDef, int expectedCount, VarBinding binding)
    {
    assertEquals( "Test cases including " + binding, expectedCount, getTestCasesIncluding( testDef, binding));
    }

  /**
   * Reports a failure if the number of test cases including the given tuple does not equal the expected value.
   */
  public static void assertIncluded( FunctionTestDef testDef, int expectedCount, Tuple tuple)
    {
    assertEquals( "Test cases including " + tuple, expectedCount, getTestCasesIncluding( testDef, tuple));
    }

  /**
   * For each of the given tuples, reports a failure if the number of test cases including the tuple does not equal the expected value.
   */
  public static void assertIncluded( FunctionTestDef testDef, int expectedCount, Iterable<Tuple> tuples)
    {
    for( Tuple tuple : tuples)
      {
      assertEquals( "Test cases including " + tuple, expectedCount, getTestCasesIncluding( testDef, tuple));
      }
    }

  /**
   * Returns a list of tuples combining the give var binding with all bindings for the other variable (set).
   */
  public static Iterable<Tuple> tuplesFor( FunctionInputDef inputDef, String var, String value, String otherVar)
    {
    List<Tuple> tuples = new ArrayList<Tuple>();

    IVarDef otherVarDef = inputDef.findVarPath( otherVar);
    List<VarDef> otherVarDefs =
      otherVarDef.getMembers() == null
      ? Arrays.asList( (VarDef) otherVarDef)
      : IteratorUtils.toList( new VarDefIterator( otherVarDef.getMembers()));

    TupleBuilder builder = tupleFor( inputDef);
    for( VarDef varDef : otherVarDefs)
      {
      for( Iterator<VarValueDef> values = varDef.getValues(); values.hasNext(); )
        {
        tuples.add
          ( builder
            .clear()
            .bind( var, value)
            .bind( varDef.getPathName(), values.next().getName())
            .build());
        }
      }
    
    return tuples;
    }

  /**
   * Returns a new TupleBuilder
   */
  public static TupleBuilder tupleFor( FunctionInputDef inputDef)
    {
    return new TupleBuilder( inputDef);
    }

  public static class TupleBuilder
    {
    /**
     * Creates a new TupleBuilder object.
     */
    public TupleBuilder( FunctionInputDef inputDef)
      {
      inputDef_ = inputDef;
      clear();
      }

    public TupleBuilder clear()
      {
      tuple_ = new Tuple();
      return this;
      }

    public TupleBuilder bind( String var, String value)
      {
      VarDef varDef = inputDef_.findVarDefPath( var);
      tuple_.add( new VarBindingDef( varDef, varDef.getValue( value)));
      return this;
      }

    public Tuple build()
      {
      return tuple_;
      }

    private FunctionInputDef inputDef_;
    private Tuple tuple_;
    }
  }
