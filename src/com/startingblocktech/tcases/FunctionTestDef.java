//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Defines test cases for a specific function.
 *
 * @version $Revision$, $Date$
 */
public class FunctionTestDef
  {
  /**
   * Creates a new FunctionTestDef object.
   */
  public FunctionTestDef()
    {
    this( null);
    }
  
  /**
   * Creates a new FunctionTestDef object.
   */
  public FunctionTestDef( String name)
    {
    setName( name);
    }

  /**
   * Changes the function name.
   */
  public void setName( String name)
    {
    name_ = name;
    }

  /**
   * Returns the function name.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Adds a new test case.
   */
  public FunctionTestDef addTestCase( TestCase testCase)
    {
    assert testCase != null;

    if( findTestCase( testCase.getId()) >= 0)
      {
      throw new IllegalStateException( "Test=" + testCase.getId() + "already defined for function=" + getName());
      }
    
    testCases_.add( testCase);
    return this;
    }

  /**
   * Removes a test case.
   */
  public FunctionTestDef removeTestCase( int id)
    {
    int i = findTestCase( id);
    if( i >= 0)
      {
      testCases_.remove(i);
      }

    return this;
    }

  /**
   * Returns the test case with the given id.
   */
  public TestCase getTestCase( int id)
    {
    int i = findTestCase( id);
    return i >= 0? testCases_.get(i) : null;
    }

  /**
   * Returns the test cases for this function.
   */
  public Iterator<TestCase> getTestCases()
    {
    return testCases_.iterator();
    }

  /**
   * Returns the index of the test case with the given id.
   */
  protected int findTestCase( int id)
    {
    int testCount = testCases_.size();
    int i;
    for( i = 0; i < testCount && testCases_.get(i).getId() != id; i++);
    return i < testCount? i : -1;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .toString();
    }

  private String name_;
  private List<TestCase> testCases_ = new ArrayList<TestCase>();
  }

