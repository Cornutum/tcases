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
public class TCaseBuilder
  {
  /**
   * Creates a new TCaseBuilder object.
   */
  public TCaseBuilder()
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
  public TCaseBuilder start()
    {
    testCase_ = new TestCase(0);
    return this;
    }

  /**
   * Changes the test case id.
   */
  public TCaseBuilder id( int id)
    {
    testCase_.setId( id);
    return this;
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TCaseBuilder bind( VarBinding binding)
    {
    testCase_.addVarBinding( binding);
    return this;
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TCaseBuilder bind( String name, String value, String type, boolean isValid)
    {
    VarBinding binding = new VarBinding( name, type, value);
    binding.setValueValid( isValid);
    return bind( binding);
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TCaseBuilder bind( String name, String value)
    {
    return bind( name, value, IVarDef.ARG);
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TCaseBuilder bind( String name, String value, String type)
    {
    return bind( name, value, type, true);
    }

  /**
   * Adds a variable binding to the test case.
   */
  public TCaseBuilder bind( String name, String value, boolean isValid)
    {
    return bind( name, value, IVarDef.ARG, isValid);
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

