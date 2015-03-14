//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;

/**
 * Generates {@link TestCase test cases} for a {@link FunctionInputDef function}.
 *
 */
public interface ITestCaseGenerator
  {
  /**
   * Changes the random number sequence seed for this generator.
   */
  void setRandomSeed( Long seed);

  /**
   * Returns a set of {@link TestCase test cases} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  FunctionTestDef getTests( FunctionInputDef inputDef, FunctionTestDef baseTests);  
  }
