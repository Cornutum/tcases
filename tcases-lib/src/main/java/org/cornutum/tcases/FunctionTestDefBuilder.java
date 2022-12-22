//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link FunctionTestDef} instances.
 *
 */
public class FunctionTestDefBuilder extends AnnotatedBuilder<FunctionTestDefBuilder>
  {
  /**
   * Creates a new builder for a FunctionTestDef with the given name.
   */
  public static FunctionTestDefBuilder with( String name)
    {
    return new FunctionTestDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given FunctionTestDef.
   */
  public static FunctionTestDefBuilder with( FunctionTestDef functionTestDef)
    {
    return new FunctionTestDefBuilder( functionTestDef);
    }


  /**
   * Creates a new FunctionTestDefBuilder object.
   */
  public FunctionTestDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new FunctionTestDefBuilder object.
   */
  public FunctionTestDefBuilder( FunctionTestDef function)
    {
    start( function);
    }

  /**
   * Returns the current function test definition.
   */
  public FunctionTestDef build()
    {
    return functionTestDef_;
    }

  /**
   * Starts building a new function test definition.
   */
  public FunctionTestDefBuilder start( FunctionTestDef function)
    {
    functionTestDef_ =
      Optional.ofNullable( function)
      .map( f ->
            FunctionTestDefBuilder.with( f.getName())
            .testCases(
              toStream( f.getTestCases())
              .map( tc -> TestCaseBuilder.with( tc).build()))
            .annotations( f)
            .build())
      .orElse( new FunctionTestDef( "F"));

    return this;
    }

  /**
   * Starts building a new function test definition.
   */
  public FunctionTestDefBuilder start()
    {
    return start( null);
    }

  /**
   * Changes the function name.
   */
  public FunctionTestDefBuilder name( String name)
    {
    functionTestDef_.setName( name);
    return this;
    }

  /**
   * Adds function test cases.
   */
  public FunctionTestDefBuilder testCases( TestCase... testCases)
    {
    for( TestCase testCase : testCases)
      {
      functionTestDef_.addTestCase( testCase);
      }
    return this;
    }

  /**
   * Adds function test cases.
   */
  public FunctionTestDefBuilder testCases( Iterable<TestCase> testCases)
    {
    for( TestCase testCase : testCases)
      {
      functionTestDef_.addTestCase( testCase);
      }
    return this;
    }

  /**
   * Adds function test cases.
   */
  public FunctionTestDefBuilder testCases( Stream<TestCase> testCases)
    {
    testCases.forEach( testCase -> functionTestDef_.addTestCase( testCase));
    return this;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
  protected Annotated getAnnotated()
    {
    return functionTestDef_;
    }

  FunctionTestDef functionTestDef_;
  }
