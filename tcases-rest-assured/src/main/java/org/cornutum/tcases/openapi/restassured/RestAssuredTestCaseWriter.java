//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;
import static org.cornutum.tcases.openapi.testwriter.TestWriterUtils.*;

/**
 * Writes the source code for REST Assured test cases that execute API requests.
 */
public class RestAssuredTestCaseWriter implements TestCaseWriter
  {
  /**
   * Creates a new RestAssuredTestCaseWriter instance.
   */
  public RestAssuredTestCaseWriter()
    {
    }
  
  /**
   * Writes the dependencies for target test cases to the given stream.
   */
  public void writeDependencies( String testName, IndentedWriter targetWriter)
    {
    targetWriter.println();
    targetWriter.println( "import static io.restassured.RestAssured.*;");
    targetWriter.println( "import static org.hamcrest.Matchers.*;");
    }

  /**
   * Writes the declarations for target test cases to the given stream.
   */
  public void writeDeclarations( String testName, IndentedWriter targetWriter)
    {
    // By default, none
    }
  
  /**
   * Writes a target test case to the given stream.
   */
  public void writeTestCase( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    targetWriter.println( "given()");
    targetWriter.indent();
    writeParams( testName, requestCase, targetWriter);
    targetWriter.unindent();

    targetWriter.println( ".when()");
    targetWriter.indent();
    writeRequest( testName, requestCase, targetWriter);
    targetWriter.unindent();

    targetWriter.println( ".then()");
    targetWriter.indent();
    writeExpectResponse( testName, requestCase, targetWriter);
    targetWriter.println( ";");
    targetWriter.unindent();
    }

  /**
   * Writes the closing for target test cases the given stream.
   */
  public void writeClosing( String testName, IndentedWriter targetWriter)
    {
    // By default, none
    }
  
  /**
   * Writes request parameter definitions for a target test case to the given stream.
   */
  protected void writeParams( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    for( ParamData param : requestCase.getParams())
      {
      writeParam( testName, param, targetWriter);
      }
    }
  
  /**
   * Writes a request parameter definition for a target test case to the given stream.
   */
  protected void writeParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    switch( param.getLocation())
      {
      case QUERY:
        {
        writeQueryParam( testName, param, targetWriter);
        break;
        }
      
      case PATH:
        {
        writePathParam( testName, param, targetWriter);
        break;
        }
      
      case HEADER:
        {
        writeHeaderParam( testName, param, targetWriter);
        break;
        }
      
      case COOKIE:
        {
        writeCookieParam( testName, param, targetWriter);
        break;
        }
      }
    }
  
  /**
   * Writes a query parameter definition for a target test case to the given stream.
   */
  protected void writeQueryParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    getQueryParameters( param, true).stream()
      .forEach( entry -> targetWriter.println( String.format( ".queryParam( \"%s\", \"%s\")", entry.getKey(), entry.getValue())));
    }
  
  /**
   * Writes a path parameter definition for a target test case to the given stream.
   */
  protected void writePathParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    targetWriter.println( String.format( ".pathParam( \"%s\", \"%s\")", param.getName(), getPathParameterValue( param)));
    }
  
  /**
   * Writes a header parameter definition for a target test case to the given stream.
   */
  protected void writeHeaderParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    }
  
  /**
   * Writes a cookie parameter definition for a target test case to the given stream.
   */
  protected void writeCookieParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    }
  
  /**
   * Writes the request definition for a target test case to the given stream.
   */
  protected void writeRequest( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    targetWriter.println(
      String.format(
        ".request( \"%s\", \"%s%s\")",
        requestCase.getOperation().toUpperCase(),
        StringUtils.stripEnd( String.valueOf( requestCase.getServer()), "/"),
        requestCase.getPath()));
    }
  
  /**
   * Writes response expectations for a target test case to the given stream.
   */
  protected void writeExpectResponse( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    if( requestCase.isFailure())
      {
      targetWriter.println( String.format( "// %s", requestCase.getInvalidInput()));
      targetWriter.println( ".statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))");
      }
    else
      {
      targetWriter.println( ".statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))");
      }
    }
  }
