//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.AuthDef;
import org.cornutum.tcases.openapi.resolver.DataValue;
import org.cornutum.tcases.openapi.resolver.EncodingData;
import org.cornutum.tcases.openapi.resolver.MessageData;
import org.cornutum.tcases.openapi.resolver.ObjectValue;
import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.test.MediaRange;
import org.cornutum.tcases.openapi.testwriter.TestWriterException;
import org.cornutum.tcases.openapi.testwriter.BaseTestCaseWriter;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueBinary;
import org.cornutum.tcases.openapi.testwriter.encoder.FormUrlEncoder;
import org.cornutum.tcases.openapi.testwriter.encoder.SimpleValueEncoder;

import static org.cornutum.tcases.openapi.testwriter.TestWriterUtils.*;
import static org.cornutum.tcases.openapi.testwriter.java.TestCaseWriterUtils.*;

import org.apache.commons.lang3.StringUtils;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.stream.Collectors.joining;

/**
 * Writes the source code for REST Assured test cases that execute API requests.
 */
public class RestAssuredTestCaseWriter extends BaseTestCaseWriter
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
    if( getDepends().validateResponses())
      {
      targetWriter.println( "import java.util.Map;");
      targetWriter.println( "import static java.util.stream.Collectors.toMap;");
      targetWriter.println();
      targetWriter.println( "import io.restassured.http.Header;");
      targetWriter.println( "import io.restassured.response.Response;");
      targetWriter.println();
      targetWriter.println( "import org.cornutum.tcases.openapi.test.ResponseValidator;");
      targetWriter.println();
      }
    if( getDepends().dependsMultipart())
      {
      targetWriter.println( "import io.restassured.builder.MultiPartSpecBuilder;");
      targetWriter.println();
      }
    targetWriter.println( "import org.hamcrest.Matcher;");
    targetWriter.println( "import static io.restassured.RestAssured.*;");
    targetWriter.println( "import static org.hamcrest.Matchers.*;");
    }

  /**
   * Writes the declarations for target test cases to the given stream.
   */
  @Override
  public void writeDeclarations( String testName, IndentedWriter targetWriter)
    {
    if( validateResponses())
      {
      writeResponseValidatorDef( testName, targetWriter);
      }
    }
  
  /**
   * Writes a target test case to the given stream.
   */
  @Override
  public void writeTestCase( String testName, URI testServer, RequestCase requestCase, IndentedWriter targetWriter)
    {
    try
      {
      if( getDepends().validateResponses())
        {
        targetWriter.println( "Response response =");
        targetWriter.indent();
        }

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

      if( getDepends().validateResponses())
        {
        targetWriter.unindent();

        targetWriter.println( ".extract()");
        targetWriter.indent();
        targetWriter.println( ".response()");
        }

      targetWriter.println( ";");
      targetWriter.unindent();

      if( getDepends().validateResponses())
        {
        targetWriter.unindent();

        targetWriter.println();
        targetWriter.println(
          String.format(
            "responseValidator.assertBodyValid( %s, %s, response.statusCode(), response.getContentType(), response.asString());",
            stringLiteral( requestCase.getOperation().toUpperCase()),
            stringLiteral( requestCase.getPath())));
        
        targetWriter.println(
          String.format(
            "responseValidator.assertHeadersValid( %s, %s, response.statusCode(), responseHeaders( response));",
            stringLiteral( requestCase.getOperation().toUpperCase()),
            stringLiteral( requestCase.getPath())));
        }
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
    writeStatusCodeMatcherDef( testName, targetWriter, getDepends());
    writeTestServerDef( testName, targetWriter, getDepends());
    writeAuthCredentialsDef( testName, targetWriter, getDepends());
    writeResponseHeadersDef( testName, targetWriter, getDepends());
    }

  /**
   * Writes the definition of standard methods to extract response headers.
   */
  protected void writeResponseHeadersDef( String testName, IndentedWriter targetWriter, Depends dependencies)
    {
    if( dependencies.validateResponses())
      {
      targetWriter.println();
      targetWriter.println( "private static Map<String,String> responseHeaders( Response response) {");
      targetWriter.indent();
      targetWriter.println( "return");
      targetWriter.indent();
      targetWriter.println( "response.getHeaders().asList().stream()");
      targetWriter.println( ".collect( toMap( Header::getName, Header::getValue));");
      targetWriter.unindent();
      targetWriter.unindent();
      targetWriter.println( "}");
      }
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
      .forEach( entry -> {
        String queryParamFormat =
          entry.getValue() == null
          ? ".queryParam( %s, (String) %s)"
          : ".queryParam( %s, %s)";
          
        targetWriter.println( String.format( queryParamFormat, stringLiteral( entry.getKey()), stringLiteral( entry.getValue())));
        });
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
    }
  
  /**
   * Writes the server URI for a target test case to the given stream.
   */
  protected void writeServer( String testName, URI testServer, RequestCase requestCase, IndentedWriter targetWriter)
    {
    Optional<String> serverUri = serverUri( testServer, requestCase);
    targetWriter.println( String.format(".baseUri( %s)", forTestServer( serverUri)));

    if( !serverUri.isPresent())
      {
      getDepends().setDependsServer();
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

            MediaRange mediaType = MediaRange.of( body.getMediaType());
            targetWriter.println( String.format( ".contentType( %s)", stringLiteral( mediaType)));

            // Write binary value?
            if( "application/octet-stream".equals( mediaType.base()))
              {
              // Yes
              writeBodyBinary( testName, value, targetWriter);
              }

            // Write form value?
            else if( "application/x-www-form-urlencoded".equals( mediaType.base()))
              {
              writeBodyForm( testName, body, targetWriter);
              }

            // Write multipart form value?
            else if( "multipart/form-data".equals( mediaType.base()))
              {
              writeBodyMultipart( testName, body, targetWriter);
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
  protected void writeBodyBinary( String testName, DataValue<?> value, IndentedWriter targetWriter)
    {
    List<String> segments = byteInitializerFor( value);

    // If small value...
    if( segments.size() == 1)
      {
      // ... write a single line
      targetWriter.println( String.format( ".request().body( new byte[]{%s})", segments.get(0)));
      }
    else
      {
      // Otherwise, write as multiple lines.
      targetWriter.println( ".request().body(");
      targetWriter.indent();
      targetWriter.println( "new byte[] {");

      targetWriter.indent();
      for( String segment : segments)
        {
        targetWriter.println( segment);
        }
      targetWriter.unindent();
        
      targetWriter.println( "})");
      targetWriter.unindent();
      }
    }
  
  /**
   * Writes the request body as an <CODE>application/x-www-form-urlencoded</CODE> form for a target test case to the given stream.
   */
  protected void writeBodyForm( String testName, MessageData body, IndentedWriter targetWriter)
    {
    FormUrlEncoder.encode( body.getValue(), body.getEncodings(), false)
      .stream()
      .forEach( entry -> {
        String formParamFormat =
          entry.getValue() == null
          ? ".formParam( %s, (String) %s)"
          : ".formParam( %s, %s)";

        targetWriter.println(
          String.format(
            formParamFormat,
            stringLiteral( entry.getKey()),
            stringLiteral( entry.getValue())));
        });
    }
  
  /**
   * Writes the request body as <CODE>multipart/form-data</CODE> form for a target test case to the given stream.
   */
  protected void writeBodyMultipart( String testName, MessageData body, IndentedWriter targetWriter)
    {
    // Multipart forms apply only to object values. Non-object values, which may be supplied by failure test cases, are all
    // handled as "empty body".
    if( body.getValue().getType() == DataValue.Type.OBJECT)
      {
      ObjectValue objectValue = (ObjectValue) body.getValue();
      objectValue.getValue().forEach( (property, value) -> writeMultipartPart( property, value, body.getEncodings().get( property), targetWriter));
      }
    }
  
  /**
   * Writes the content of the <CODE>multipart/form-data</CODE> part for the given body object property.
   */
  protected void writeMultipartPart( String property, DataValue<?> value, EncodingData encoding, IndentedWriter targetWriter)
    {
    MediaRange contentType = MediaRange.of( encoding.getContentType());
    
    targetWriter.println( ".multiPart(");
    targetWriter.indent();

    if( "application/octet-stream".equals( contentType.base()))
      {
      List<String> segments = byteInitializerFor( value);

      // If small value...
      if( segments.size() == 1)
        {
        // ... write a single line
        targetWriter.println( String.format( "new MultiPartSpecBuilder( new byte[]{%s})", segments.get(0)));
        }
      else
        {
        // Otherwise, write as multiple lines.
        targetWriter.println( "new MultiPartSpecBuilder(");
        targetWriter.indent();
        targetWriter.println( "new byte[] {");

        targetWriter.indent();
        for( String segment : segments)
          {
          targetWriter.println( segment);
          }
        targetWriter.unindent();
        
        targetWriter.println( "})");
        targetWriter.unindent();
        }
      }

    else
      {
      String partData;
      if( "application/x-www-form-urlencoded".equals( contentType.base()))
        {
        partData = FormUrlEncoder.toForm( value);
        }
      else
        {
        partData =
          getConverter( contentType)
          .orElseThrow( () -> new TestWriterException( String.format( "No serializer defined for contentType=%s", contentType)))
          .convert( value);
        }

      targetWriter.println( String.format( "new MultiPartSpecBuilder( %s)", stringLiteral( partData)));      
      }
    
    targetWriter.println( String.format( ".controlName( %s)", stringLiteral( property)));
    targetWriter.println( String.format( ".mimeType( %s)", stringLiteral( contentType)));

    encoding.getHeaders().stream()
      .forEach( headerData -> {
        targetWriter.println(
          String.format(
            ".header( %s, %s)",
            stringLiteral( headerData.getName()),
            stringLiteral( SimpleValueEncoder.encode( headerData.getValue(), headerData.isExploded()))));
        });
    
    targetWriter.println( ".emptyFileName()");
    targetWriter.println( ".build())");
    targetWriter.unindent();
    }

  /**
   * Returns the initializer code for the byte array representing the given data value.
   */
  private List<String> byteInitializerFor( DataValue<?> value)
    {
    final int lineSize = 16;

    List<String> segments = new ArrayList<String>();
    byte[] bytes = DataValueBinary.toBytes( value);

    int from;
    for( from = 0; bytes.length - from > lineSize; from += lineSize)
      {
      segments.add( String.format( "%s,", byteInitializerFor( bytes, from, from + lineSize)));
      }
    segments.add( byteInitializerFor( bytes, from, bytes.length));

    return segments;
    }

  /**
   * Returns the initializer code for the given byte array segment.
   */
  private String byteInitializerFor( byte[] bytes, int start, int end)
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
  }
