//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import java.util.Iterator;

/**
 * Defines a set of {@link ITestCaseGenerator test case generators}.
 *
 * @version $Revision$, $Date$
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
  
  }
