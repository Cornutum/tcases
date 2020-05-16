//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

/**
 * Creates a new {@link TestWriter} instance.
 */
public interface TestWriterFactory
  {
  /**
   * Creates a new {@link TestWriter} instance.
   */
  public TestWriter<?,?> createTestWriter( TestCaseWriter testCaseWriter);
  }
