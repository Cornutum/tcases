//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.java;

import org.cornutum.tcases.io.IndentedWriter;


/**
 * Defines common methods for generating standard Java methods used by {@link org.cornutum.tcases.openapi.testwriter.TestCaseWriter} implementations.
 */
public final class TestCaseWriterUtils
  {
  /**
   * Creates a new TestCaseWriterUtils instance.
   */
  private TestCaseWriterUtils()
    {
    // Static methods only
    }

  /**
   * Writes the definition of standard status code matcher methods to the given stream. Note: this generates a runtime dependency
   * on <A href="http://hamcrest.org/JavaHamcrest/distributables#previous-versions-of-hamcrest">hamcrest.jar</A>.
   */
  public static void writeStatusCodeMatcherDef( String testName, IndentedWriter targetWriter)
    {
    targetWriter.println();
    targetWriter.println( "private static Matcher<Integer> isSuccess() {");
    targetWriter.indent();
    targetWriter.println( "return allOf( greaterThanOrEqualTo(200), lessThan(300));");
    targetWriter.unindent();
    targetWriter.println( "}");
    targetWriter.println();
    targetWriter.println( "private static Matcher<Integer> isBadRequest() {");
    targetWriter.indent();
    targetWriter.println( "return allOf( greaterThanOrEqualTo(400), lessThan(500));");
    targetWriter.unindent();
    targetWriter.println( "}");
    }

  /**
   * Writes the definition of standard methods for runtime specification of the API server URI to the given stream.
   */
  public static void writeTestServerDef( String testName, IndentedWriter targetWriter)
    {
    targetWriter.println();
    targetWriter.println( "private static String forTestServer() {");
    targetWriter.indent();
    targetWriter.println( "return forTestServer( null);");
    targetWriter.unindent();
    targetWriter.println( "}");
    targetWriter.println();
    targetWriter.println( "private static String forTestServer( String defaultUri) {");
    targetWriter.indent();
    targetWriter.println( "String testServer = tcasesApiServer();");
    targetWriter.println( "return");
    targetWriter.indent();
    targetWriter.println( "defaultUri == null || !testServer.isEmpty()");
    targetWriter.println( "? testServer");
    targetWriter.println( ": defaultUri;");
    targetWriter.unindent();
    targetWriter.unindent();
    targetWriter.println( "}");
    targetWriter.println();
    targetWriter.println( "private static String tcasesApiServer() {");
    targetWriter.indent();
    targetWriter.println( "String uri = System.getProperty( \"tcasesApiServer\");");
    targetWriter.println( "return uri == null? \"\" : uri.trim();");
    targetWriter.unindent();
    targetWriter.println( "}");
    }
  }
