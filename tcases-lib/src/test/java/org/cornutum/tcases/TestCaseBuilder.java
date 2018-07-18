//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

/**
 * Builds {@link TestCase} instances.
 *
 */
public class TestCaseBuilder
  {
  /**
   * Creates a new TCaseBuilder object.
   */
  public TestCaseBuilder()
    {
    start();
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
    testCase_ = new TestCase(0);
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
   * Adds a variable binding to the test case.
   */
  public TestCaseBuilder bind( String name, String value, String type, boolean isValid)
    {
    VarBinding binding = new VarBinding( name, type, value);
    binding.setValueValid( isValid);
    return bind( binding);
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TestCaseBuilder bind( String name, String value)
    {
    return bind( name, value, IVarDef.ARG);
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TestCaseBuilder bind( String name, String value, String type)
    {
    return bind( name, value, type, true);
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TestCaseBuilder bind( String name, String value, boolean isValid)
    {
    return bind( name, value, IVarDef.ARG, isValid);
    }

  /**
   * Declares a variable "not applicable" to the test case.
   */
  public TestCaseBuilder notApplicable( String name)
    {
    return notApplicable( name, IVarDef.ARG);
    }

  /**
   * Declares a variable "not applicable" to the test case.
   */
  public TestCaseBuilder notApplicable( String name, String type)
    {
    return bind( new VarNaBinding( name, type));
    }

  /**
   * Resets ids for the given sequence of test cases.
   */
  public static TestCase[] sequence( TestCase ... testCases)
    {
    for( int i = 0; i < testCases.length; i++)
      {
      testCases[i].setId( i);
      }
    return testCases;
    }

  TestCase testCase_;
  }

