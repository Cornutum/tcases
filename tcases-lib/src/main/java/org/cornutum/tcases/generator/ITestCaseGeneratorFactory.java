//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

/**
 * Creates {@link ITestCaseGenerator} instances
 *
 */
public interface ITestCaseGeneratorFactory
  {
  /**
   * Creates a new {@link ITestCaseGenerator} instance. Default properties of the new generator may
   * be defined by the given <CODE>defaultGenerator</CODE>. If <CODE>defaultGenerator</CODE> is null,
   * no default properties are defined.
   */
  ITestCaseGenerator newGenerator( ITestCaseGenerator defaultGenerator);
  }
