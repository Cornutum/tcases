//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import java.util.Iterator;

/**
 * Defines a set of {@link ITestCaseGenerator test case generators}.
 *
 */
public interface IGeneratorSet
  {
  /**
   * Returns the test case generator for the given system function.
   */
  ITestCaseGenerator getGenerator( String functionName);

  /**
   * Returns all test case generators in this set.
   */
  Iterator<ITestCaseGenerator> getGenerators();

  /**
   * Returns the set of system function names associated with generators in
   * this set.
   */
  String[] getGeneratorFunctions();
  }
