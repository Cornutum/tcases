//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.CloneableType;

import java.util.List;

/**
 * Generates {@link TestCase test cases} for a {@link FunctionInputDef function}.
 *
 */
public interface ITestCaseGenerator extends CloneableType<ITestCaseGenerator>
  {
  /**
   * Changes the random number sequence seed for this generator.
   */
  void setRandomSeed( Long seed);

  /**
   * Returns the random number sequence seed for this generator.
   */
  Long getRandomSeed();

  /**
   * Returns a set of {@link TestCase test cases} for the given function input definition.
   * If the given base test definition is non-null, returns a set of new test cases
   * that extend the base tests.
   */
  List<ITestCaseDef> getTests( FunctionInputDef inputDef, FunctionTestDef baseTests);  
  }
