//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.ApiKeyDef;
import org.cornutum.tcases.openapi.resolver.AuthDef;
import org.cornutum.tcases.openapi.resolver.AuthDefVisitor;
import org.cornutum.tcases.openapi.resolver.BinaryValue;
import org.cornutum.tcases.openapi.resolver.DataValue;
import org.cornutum.tcases.openapi.resolver.HttpBasicDef;
import org.cornutum.tcases.openapi.resolver.HttpBearerDef;
import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.testwriter.TestCaseContentWriter;
import org.cornutum.tcases.openapi.testwriter.TestWriterException;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueBinary;
import org.cornutum.tcases.openapi.testwriter.encoder.FormUrlEncoder;
import static org.cornutum.tcases.openapi.testwriter.TestWriterUtils.*;
import static org.cornutum.tcases.openapi.testwriter.java.TestCaseWriterUtils.*;

import org.apache.commons.lang3.StringUtils;

import java.net.URI;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.stream.Collectors.joining;

/**
 * Writes the source code for REST Assured test cases that execute API requests.
 */
public class RestAssuredTestCaseWriter extends TestCaseContentWriter
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
  @Override
public void writeDependencies( String testName, IndentedWriter targetWriter)
    {
    targetWriter.println();
    targetWriter.println( "import org.hamcrest.Matcher;");
    targetWriter.println( "import static io.restassured.RestAssured.*;");
    targetWriter.println( "import static org.hamcrest.Matchers.*;");

    depends_ = new Depends();
    }

  /**
   * Writes the declarations for target test cases to the given stream.
   */
  @Override
public void writeDeclarations( String testName, IndentedWriter targetWriter)
    {
    // By default, none
    }
  
  /**
   * Writes a target test case to the given stream.
   */
  @Override
public void writeTestCase( String testName, URI testServer, RequestCase requestCase, IndentedWriter targetWriter)
    {
    try
      {
      targetWriter.println( "given()");
      targetWriter.indent();
      writeServer( testName, testServer, requestCase, targetWriter);
      writeAuthDefs( testName, requestCase, targetWriter);
      writeParams( testName, requestCase, targetWriter);
      writeBody( testName, requestCase, targetWriter);
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
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "Can't write test case=%s", requestCase), e);
      }
    }

  /**
   * Writes the closing for target test cases to the given stream.
   */
  @Override
public void writeClosing( String testName, IndentedWriter targetWriter)
    {
    writeStatusCodeMatcherDef( testName, targetWriter, depends_);
    writeTestServerDef( testName, targetWriter, depends_);
    writeAuthCredentialsDef( testName, targetWriter, depends_);
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
    getQueryParameters( param).stream()
      .forEach( entry -> targetWriter.println( String.format( ".queryParam( %s, %s)", stringLiteral( entry.getKey()), stringLiteral( entry.getValue()))));
    }
  
  /**
   * Writes a path parameter definition for a target test case to the given stream.
   */
  protected void writePathParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    targetWriter.println( String.format( ".pathParam( %s, %s)", stringLiteral( param.getName()), stringLiteral( getPathParameterValue( param))));
    }
  
  /**
   * Writes a header parameter definition for a target test case to the given stream.
   */
  protected void writeHeaderParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    getHeaderParameterValue( param)
      .ifPresent( value -> targetWriter.println( String.format( ".header( %s, %s)", stringLiteral( param.getName()), stringLiteral( value))));
    }
  
  /**
   * Writes a cookie parameter definition for a target test case to the given stream.
   */
  protected void writeCookieParam( String testName, ParamData param, IndentedWriter targetWriter)
    {
    getCookieParameters( param).stream()
      .forEach( entry -> targetWriter.println( String.format( ".cookie( %s, %s)", stringLiteral( entry.getKey()), stringLiteral( entry.getValue()))));
    }
  
  /**
   * Writes request authentication definitions for a target test case to the given stream.
   */
  protected void writeAuthDefs( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    for( AuthDef authDef : requestCase.getAuthDefs())
      {
      writeAuthDef( testName, authDef, targetWriter);
      }
    }
  
  /**
   * Writes a request authentication definition for a target test case to the given stream.
   */
  protected void writeAuthDef( String testName, AuthDef authDef, IndentedWriter targetWriter)
    {
    switch( authDef.getLocation())
      {
      case QUERY:
        {
        targetWriter.println( String.format( ".queryParam( %s, %s)", stringLiteral( authDef.getName()), "tcasesApiKey()"));
        break;
        }
      
      case HEADER:
        {
        targetWriter.println( String.format( ".header( %s, %s)", stringLiteral( authDef.getName()), headerValueOf( authDef)));
        break;
        }
      
      case COOKIE:
        {
        targetWriter.println( String.format( ".cookie( %s, %s)", stringLiteral( authDef.getName()), "tcasesApiKey()"));
        break;
        }

      default:
        {
        throw new IllegalStateException( String.format( "Invalid location for authentication value=%s", authDef));
        }
      }

    authDef.accept( authDependsVisitor_);
    }
  
  /**
   * Writes the server URI for a target test case to the given stream.
   */
  protected void writeServer( String testName, URI testServer, RequestCase requestCase, IndentedWriter targetWriter)
    {
    Optional<String> serverUri =
      Optional.of(
        StringUtils.stripEnd(
        Objects.toString( testServer, Objects.toString( requestCase.getServer(), "")),
        "/"))
      .filter( StringUtils::isNotBlank);

    targetWriter.println
      ( String.format(
        ".baseUri( forTestServer(%s))",
        serverUri.map( uri -> String.format( " %s", stringLiteral( uri))) .orElse( "")));

    if( !serverUri.isPresent())
      {
      depends_.setDependsServer();
      }
    }
  
  /**
   * Writes the request definition for a target test case to the given stream.
   */
  protected void writeRequest( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    targetWriter.println(
      String.format(
        ".request( %s, %s)",
        stringLiteral( requestCase.getOperation().toUpperCase()),
        stringLiteral( requestCase.getPath())));
    }
  
  /**
   * Writes the request body for a target test case to the given stream.
   */
  protected void writeBody( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    Optional.ofNullable( requestCase.getBody())
      .ifPresent( body -> {
        Optional.ofNullable( body.getValue())
          .ifPresent( value -> {

            String mediaType = body.getMediaType();
            targetWriter.println( String.format( ".contentType( %s)", stringLiteral( mediaType)));

            byte[] bytes = 
              "application/octet-stream".equals( mediaType) || (mediaType == null && value.getClass().equals( BinaryValue.class))
              ? DataValueBinary.toBytes( value)
              : null;

            // Write binary value?
            if( bytes != null)
              {
              // Yes
              writeBodyBinary( testName, bytes, targetWriter);
              }

            // Write form value?
            else if( "application/x-www-form-urlencoded".equals( mediaType))
              {
              writeBodyForm( testName, value, targetWriter);
              }
            else
              {
              // No, serialize body value according to media type
              targetWriter.println(
                String.format(
                  ".request().body( %s)",
                  stringLiteral(
                    getConverter( mediaType)
                    .orElseThrow( () -> new TestWriterException( String.format( "No serializer defined for mediaType=%s", mediaType)))
                    .convert( value))));
              }
            });
        });
    }
  
  /**
   * Writes the request body as a byte array for a target test case to the given stream.
   */
  protected void writeBodyBinary( String testName, byte[] bytes, IndentedWriter targetWriter)
    {
    // If small value...
    final int lineSize = 16;
    if( bytes.length <= lineSize)
      {
      // ... write a single line
      targetWriter.println( String.format( ".request().body( new byte[]{%s})", initializerFor( bytes, 0, bytes.length)));
      }
    else
      {
      // Otherwise, write as multiple lines.
      targetWriter.println( ".request().body(");
      targetWriter.indent();
      targetWriter.println( "new byte[] {");
      targetWriter.indent();

      int from;
      for( from = 0; bytes.length - from > lineSize; from += lineSize)
        {
        targetWriter.println( String.format( "%s,", initializerFor( bytes, from, from + lineSize)));
        }
      targetWriter.println( initializerFor( bytes, from, bytes.length));
        
      targetWriter.unindent();
      targetWriter.println( "})");
      targetWriter.unindent();
      }
    }
  
  /**
   * Writes the request body as an x-www-form-urlencoded form for a target test case to the given stream.
   */
  protected void writeBodyForm( String testName, DataValue<?> value, IndentedWriter targetWriter)
    {
    FormUrlEncoder.encode( value, false)
      .stream()
      .forEach( entry -> {
        targetWriter.println(
          String.format(
            ".formParam( %s, %s)",
            stringLiteral( entry.getKey()),
            stringLiteral( entry.getValue())));
        });
    }

  /**
   * Returns the initializer code for the given byte array segment.
   */
  private String initializerFor( byte[] bytes, int start, int end)
    {
    return
      IntStream.range( start, end)
      .mapToObj( i -> StringUtils.leftPad( String.valueOf( bytes[i]), 4))
      .collect( joining( ","));
    }

  /**
   * Writes response expectations for a target test case to the given stream.
   */
  protected void writeExpectResponse( String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    if( requestCase.isFailure())
      {
      targetWriter.println( String.format( "// %s", requestCase.getInvalidInput()));
      targetWriter.println( String.format( ".statusCode( %s)", requestCase.isAuthFailure()? "isUnauthorized()" : "isBadRequest()"));
      }
    else
      {
      targetWriter.println( ".statusCode( isSuccess())");
      }
    }

  private Depends depends_;
  private AuthDependsVisitor authDependsVisitor_ = new AuthDependsVisitor();

  /**
   * Records compile-time dependencies for an authentication definition.
   */
  private class AuthDependsVisitor implements AuthDefVisitor
    {

    @Override
    public void visit( ApiKeyDef authDef)
      {
      depends_.setDependsApiKey();
      }

    @Override
    public void visit( HttpBasicDef authDef)
      {
      depends_.setDependsHttpBasic();
      }

    @Override
    public void visit( HttpBearerDef authDef)
      {
      depends_.setDependsHttpBearer();
      }
    }

  }
