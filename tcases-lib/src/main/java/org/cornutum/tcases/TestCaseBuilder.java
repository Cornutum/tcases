//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link TestCase} instances.
 *
 */
public class TestCaseBuilder
  {
  /**
   * Creates a new builder for a TestCase with the given id.
   */
  public static TestCaseBuilder with( int id)
    {
    return new TestCaseBuilder().id( id);
    }
  
  /**
   * Creates a new builder for the given TestCase.
   */
  public static TestCaseBuilder with( TestCase testCase)
    {
    return new TestCaseBuilder( testCase);
    }


  /**
   * Creates a new TestCaseBuilder object.
   */
  public TestCaseBuilder()
    {
    start();
    }

  /**
   * Creates a new TestCaseBuilder object.
   */
  public TestCaseBuilder( TestCase testCase)
    {
    start( testCase);
    }

  /**
   * Returns the current test case.
   */
  public TestCase build()
    {
    return testCase_;
    }

  /**
   * Starts building a new test case.
   */
  public TestCaseBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new test case.
   */
  public TestCaseBuilder start( TestCase testCase)
    {
    testCase_ =
      testCase == null
      ? new TestCase(0)
      : testCase;
    
    return this;
    }

  /**
   * Changes the test case id.
   */
  public TestCaseBuilder id( int id)
    {
    testCase_.setId( id);
    return this;
    }

  /**
   * Adds variable bindings to the test case.
   */
  public TestCaseBuilder bind( VarBinding... bindings)
    {
    for( VarBinding binding : bindings)
      {
      testCase_.addVarBinding( binding);
      }
    return this;
    }

  /**
   * Adds variable bindings to the test case.
   */
  public TestCaseBuilder bind( Iterable<VarBinding> bindings)
    {
    for( VarBinding binding : bindings)
      {
      testCase_.addVarBinding( binding);
      }
    return this;
    }

  /**
   * Adds variable bindings to the test case.
   */
  public TestCaseBuilder bind( Stream<VarBinding> bindings)
    {
    bindings.forEach( binding -> testCase_.addVarBinding( binding));
    return this;
    }

  /**
   * Adds variable bindings of the given type to the test case.
   */
  public TestCaseBuilder bind( String type, VarBinding... bindings)
    {
    for( VarBinding binding : bindings)
      {
      binding.setType( type);
      testCase_.addVarBinding( binding);
      }
    return this;
    }

  /**
   * Adds a test case annotation.
   */
  public TestCaseBuilder has( String name, Object value)
    {
    testCase_.setAnnotation( name, Objects.toString( value, null));
    return this;
    }

  /**
   * Adds a test case annotation if the given value is non-null
   */
  public TestCaseBuilder hasIf( String name, Object value)
    {
    return
      value != null
      ? has( name, value)
      : this;
    }

  /**
   * Adds a test case annotation if the given value is defined
   */
  public TestCaseBuilder hasIf( String name, Optional<Object> value)
    {
    return hasIf( name, value.orElse( null));
    }

  TestCase testCase_;
  }

