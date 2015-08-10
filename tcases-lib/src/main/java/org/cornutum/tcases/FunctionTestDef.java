//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.DefUtils.*;

import org.apache.commons.lang3.ObjectUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Defines test cases for a specific function.
 *
 */
public class FunctionTestDef extends Annotated
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
    assertIdentifier( name);
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
      throw new IllegalStateException( "Test=" + testCase.getId() + " already defined for function=" + getName());
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

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( getName())
      ^ testCases_.hashCode();
    }

  @SuppressWarnings("deprecation")
  public boolean equals( Object object)
    {
    FunctionTestDef other =
      object != null && object.getClass().equals( getClass())
      ? (FunctionTestDef) object
      : null;

    return
      other != null
      && ObjectUtils.equals( getName(), other.getName())
      && testCases_.equals( other.testCases_);
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

