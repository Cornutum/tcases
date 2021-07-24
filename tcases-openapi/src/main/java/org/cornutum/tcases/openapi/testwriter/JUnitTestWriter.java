//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;

import java.util.Optional;

/**
 * Writes Java source code for a JUnit test that executes API requests.
 */
public class JUnitTestWriter extends AnnotatedJavaTestWriter
  {
  /**
   * Creates a new JUnitTestWriter instance.
   */
  public JUnitTestWriter( TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    }

  /**
   * Writes the target test annotation dependencies to the given stream.
   */
  @Override
  protected void writeTestAnnotationDependencies( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    targetWriter.println( "import org.junit.Test;");
    }

  /**
   * Writes the annotation for a target test case to the given stream.
   */
  @Override
  protected void writeTestAnnotation( JavaTestTarget target, String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    targetWriter.println(
      String.format(
        "@Test%s",
        Optional.ofNullable( target.getTimeout())
        .filter( timeout -> timeout > 0)
        .map( timeout -> String.format( "(timeout=%s)", timeout))
        .orElse( "")));
    }
  }
