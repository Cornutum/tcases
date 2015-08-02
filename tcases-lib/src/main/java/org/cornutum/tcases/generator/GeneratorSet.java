//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Defines a set of {@link ITestCaseGenerator test case generators}.
 *
 */
public class GeneratorSet implements IGeneratorSet
  {
  /**
   * Returns the test case generator for the given system function.
   */
  public ITestCaseGenerator getGenerator( String functionName)
    {
    ITestCaseGenerator generator = generators_.get( getFunctionKey( functionName));
    return generator==null? generators_.get( ALL) : generator;
    }

  /**
   * Returns the set of system function names associated with generators in
   * this set.
   */
  public String[] getGeneratorFunctions()
    {
    String[] functions = new String[ generators_.size()];
    generators_.keySet().toArray( functions);
    return functions;
    }
  
  /**
   * Returns all test case generators in this set.
   */
  public Iterator<ITestCaseGenerator> getGenerators()
    {
    return generators_.values().iterator();
    }

  /**
   * Adds a new test case generator for the given system function.
   */
  public void addGenerator( String functionName, ITestCaseGenerator generator)
    {
    String functionKey = getFunctionKey( functionName);
    if( generators_.containsKey( functionKey))
      {
      throw new IllegalArgumentException( "Generator already defined for function=" + functionName);
      }

    if( generator != null)
      {
      generators_.put( functionKey, generator);
      }
    }

  /**
   * Changes the test case generator for the given system function.
   */
  public void setGenerator( String functionName, ITestCaseGenerator generator)
    {
    String functionKey = getFunctionKey( functionName);
    if( generator == null)
      {
      generators_.remove( functionKey);
      }
    else
      {
      generators_.put( functionKey, generator);
      }
    }

  /**
   * Returns a GeneratorSet that uses the default TupleGenerator for all functions.
   */
  public static GeneratorSet basicGenerator()
    {
    GeneratorSet genSet = new GeneratorSet();
    genSet.addGenerator( GeneratorSet.ALL, new TupleGenerator());
    return genSet;
    }

  /**
   * Returns the key used to find the test case generator for the given system function.
   */
  private String getFunctionKey( String functionName)
    {
    functionName = StringUtils.trimToNull( functionName);
    return functionName==null? ALL : functionName;
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ generators_.hashCode();
    }

  public boolean equals( Object object)
    {
    GeneratorSet other =
      object != null && object.getClass().equals( getClass())
      ? (GeneratorSet) object
      : null;

    return
      other != null
      && other.generators_.equals( generators_);
    }
  
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "functions", generators_.keySet())
      .toString();
    }

  private Map<String,ITestCaseGenerator> generators_ = new HashMap<String,ITestCaseGenerator>();

  public static final String ALL = "*";
  }

