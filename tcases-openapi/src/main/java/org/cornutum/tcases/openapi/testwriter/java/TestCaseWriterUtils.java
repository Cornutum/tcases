//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.java;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.ApiKeyDef;
import org.cornutum.tcases.openapi.resolver.AuthDef;
import org.cornutum.tcases.openapi.resolver.AuthDefVisitor;
import org.cornutum.tcases.openapi.resolver.HttpBasicDef;
import org.cornutum.tcases.openapi.resolver.HttpBearerDef;


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
  public static void writeStatusCodeMatcherDef( String testName, IndentedWriter targetWriter, Depends dependencies)
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

    if( dependencies.dependsAuth())
      {
      targetWriter.println();
      targetWriter.println( "private static Matcher<Integer> isUnauthorized() {");
      targetWriter.indent();
      targetWriter.println( "return is(401);");
      targetWriter.unindent();
      targetWriter.println( "}");
      }
    }

  /**
   * Writes the definition of standard methods for runtime specification of the API server URI to the given stream.
   */
  public static void writeTestServerDef( String testName, IndentedWriter targetWriter, Depends dependencies)
    {
    if( dependencies.dependsServer())
      {
      targetWriter.println();
      targetWriter.println( "private static String forTestServer() {");
      targetWriter.indent();
      targetWriter.println( "return forTestServer( null);");
      targetWriter.unindent();
      targetWriter.println( "}");
      }
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

  /**
   * Writes the definition of standard methods for runtime specification of authentication credentials to the given stream.
   */
  public static void writeAuthCredentialsDef( String testName, IndentedWriter targetWriter, Depends dependencies)
    {
    if( dependencies.dependsApiKey())
      {
      targetWriter.println();
      targetWriter.println( "private String tcasesApiKey() {");
      targetWriter.indent();
      targetWriter.println( "String apiKey = System.getProperty( \"tcasesApiKey\");");
      targetWriter.println( "return apiKey == null? \"\" : apiKey;");
      targetWriter.unindent();
      targetWriter.println( "}");
      }

    if( dependencies.dependsHttpBearer())
      {
      targetWriter.println();
      targetWriter.println( "private String tcasesApiBearer() {");
      targetWriter.indent();
      targetWriter.println( "String bearer = System.getProperty( \"tcasesApiBearer\");");
      targetWriter.println( "return bearer == null? \"\" : bearer;");
      targetWriter.unindent();
      targetWriter.println( "}");
      targetWriter.println();
      targetWriter.println( "private String tcasesApiBearerCredentials() {");
      targetWriter.indent();
      targetWriter.println( "return String.format( \"Bearer %s\", asToken64( tcasesApiBearer()));");
      targetWriter.unindent();
      targetWriter.println( "}");
      }

    if( dependencies.dependsHttpBasic())
      {
      targetWriter.println();
      targetWriter.println( "private String tcasesApiUser() {");
      targetWriter.indent();
      targetWriter.println( "String user = System.getProperty( \"tcasesApiUser\");");
      targetWriter.println( "return user == null? \"\" : user;");
      targetWriter.unindent();
      targetWriter.println( "}");
      targetWriter.println();
      targetWriter.println( "private String tcasesApiPassword() {");
      targetWriter.indent();
      targetWriter.println( "String password = System.getProperty( \"tcasesApiPassword\");");
      targetWriter.println( "return password == null? \"\" : password;");
      targetWriter.unindent();
      targetWriter.println( "}");
      targetWriter.println();
      targetWriter.println( "private String tcasesApiBasicCredentials() {");
      targetWriter.indent();
      targetWriter.println( "return String.format( \"Basic %s\", asToken64( String.format( \"%s:%s\", tcasesApiUser(), tcasesApiPassword())));");
      targetWriter.unindent();
      targetWriter.println( "}");
      }

    if( dependencies.dependsAuthHttp())
      {
      targetWriter.println();
      targetWriter.println( "private String asToken64( String value) {");
      targetWriter.indent();
      targetWriter.println( "try {");
      targetWriter.indent();
      targetWriter.println( "return java.util.Base64.getEncoder().encodeToString( value.getBytes( \"UTF-8\"));");
      targetWriter.unindent();
      targetWriter.println( "}");
      targetWriter.println( "catch( Exception e) {");
      targetWriter.indent();
      targetWriter.println( "    throw new IllegalArgumentException( String.format( \"Can't get Base64 token for value=%s\", value), e);");
      targetWriter.unindent();
      targetWriter.println( "}");
      targetWriter.unindent();
      targetWriter.println( "}");
      }
    }

  /**
   * Returns the Java expression that provides the header value for the given authentication definition.
   */
  public static String headerValueOf( AuthDef authDef)
    {
    return HeaderVisitor.valueOf( authDef);
    }

  /**
   * Returns the Java expression that provides the header value for an authentication definition.
   */
  private static class HeaderVisitor implements AuthDefVisitor
    {
    /**
     * Returns the Java expression that provides the header value for the given authentication definition.
     */
    private static String valueOf( AuthDef authDef)
      {
      HeaderVisitor visitor = new HeaderVisitor();
      authDef.accept( visitor);
      return visitor.value_;
      }

    public void visit( ApiKeyDef authDef)
      {
      value_ = "tcasesApiKey()";
      }

    public void visit( HttpBasicDef authDef)
      {
      value_ = "tcasesApiBasicCredentials()";
      }

    public void visit( HttpBearerDef authDef)
      {
      value_ = "tcasesApiBearerCredentials()";
      }
  
    private String value_;
    }

  /**
   * Defines the compile-time dependencies on standard methods.
   */
  public static class Depends
    {
    /**
     * Registers a dependency on a runtime API server definition.
     */
    public void setDependsServer()
      {
      dependsServer_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime API server definition.
     */
    public boolean dependsServer()
      {
      return dependsServer_;
      }
    
    /**
     * Returns if there is a dependency on a runtime API authentication.
     */
    public boolean dependsAuth()
      {
      return dependsAuth_ > 0;
      }
    
    /**
     * Returns if there is a dependency on a runtime HTTP authentication.
     */
    public boolean dependsAuthHttp()
      {
      return dependsAuthHttp_ > 0;
      }
    
    /**
     * Registers a dependency on a runtime API key credential.
     */
    public void setDependsApiKey()
      {
      dependsAuth_++;
      dependsApiKey_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime API key credential.
     */
    public boolean dependsApiKey()
      {
      return dependsApiKey_;
      }
    
    /**
     * Registers a dependency on a runtime HTTP Basic credential.
     */
    public void setDependsHttpBasic()
      {
      dependsAuth_++;
      dependsAuthHttp_++;
      dependsHttpBasic_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime HTTP Basic credential.
     */
    public boolean dependsHttpBasic()
      {
      return dependsHttpBasic_;
      }

    /**
     * Registers a dependency on a runtime HTTP Bearer credential.
     */
    public void setDependsHttpBearer()
      {
      dependsAuth_++;
      dependsAuthHttp_++;
      dependsHttpBearer_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime HTTP Bearer credential.
     */
    public boolean dependsHttpBearer()
      {
      return dependsHttpBearer_;
      }

    private boolean dependsServer_;

    private int dependsAuth_;
    private int dependsAuthHttp_;
    private boolean dependsApiKey_;
    private boolean dependsHttpBasic_;
    private boolean dependsHttpBearer_;
    }
  }
