//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

/**
 * Creates a new {@link TestCaseWriter} instance.
 */
public interface TestCaseWriterFactory
  {
  /**
   * Creates a new {@link TestCaseWriter} instance.
   */
  public TestCaseWriter createTestCaseWriter();
  }
