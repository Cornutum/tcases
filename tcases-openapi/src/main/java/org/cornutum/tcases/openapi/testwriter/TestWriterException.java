//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi.testwriter;

/**
 * Reports an error when writing an API test.
 */
public class TestWriterException extends RuntimeException
  {
  private static final long serialVersionUID = -2782384368592760469L;

  /**
   * Creates a new TestWriterException instance.
   */
  public TestWriterException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new TestWriterException instance.
   */
  public TestWriterException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
