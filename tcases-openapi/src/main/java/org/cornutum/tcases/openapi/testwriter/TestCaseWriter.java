//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;

import java.net.URI;

/**
 * Writes the source code for test cases that execute API requests.
 */
public interface TestCaseWriter
  {
  /**
   * Writes the dependencies for target test cases to the given stream.
   */
  public void writeDependencies( String testName, IndentedWriter targetWriter);

  /**
   * Writes the declarations for target test cases to the given stream.
   */
  public void writeDeclarations( String testName, IndentedWriter targetWriter);
  
  /**
   * Writes a target test case to the given stream. If non-null, the specified <CODE>testServer</CODE> supersedes
   * the URI for the API server defined by the <CODE>requestCase</CODE>.
   */
  public void writeTestCase( String testName, URI testServer, RequestCase requestCase, IndentedWriter targetWriter);

  /**
   * Writes the closing for target test cases the given stream.
   */
  public void writeClosing( String testName, IndentedWriter targetWriter);
  }
