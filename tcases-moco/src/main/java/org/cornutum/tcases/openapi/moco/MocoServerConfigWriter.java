//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestCaseException;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.testwriter.TestWriterUtils;
import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import static javax.json.stream.JsonGenerator.PRETTY_PRINTING;

/**
 * Writes JSON configuration for a MocoServer that expects requests defined by a {@link RequestTestDef}.
 */
public class MocoServerConfigWriter implements Closeable
  {
  /**
   * Creates a new MocoServerConfigWriter object that writes to standard output.
   */
  public MocoServerConfigWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new MocoServerConfigWriter object that writes to the given stream.
   */
  public MocoServerConfigWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new MocoServerConfigWriter object that writes to the given stream.
   */
  public MocoServerConfigWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given system test definition the form of a JSON document.
   */
  public void write( RequestTestDef requestCases)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( expectedConfigs( requestCases));
    }

  /**
   * Flushes the writer.
   */
  public void flush()
    {
    try
      {
      getWriter().flush();
      }
    catch( IOException ignore)
      {
      }
    }

  /**
   * Closes the writer.
   */
  public void close()
    {
    IOUtils.closeQuietly( getWriter());
    }

  /**
   * Changes the output stream for this writer.
   */
  protected void setWriter( Writer writer)
    {
    writer_ =
      writer == null
      ? writerFor( System.out)
      : writer;
    }

  /**
   * Returns the output stream for this writer.
   */
  protected Writer getWriter()
    {
    return writer_;
    }

  /**
   * Returns a JSON object that represents expectations for the given request cases.
   */
  private JsonArray expectedConfigs( RequestTestDef requestCases)
    {
    JsonArrayBuilder expected = Json.createArrayBuilder();

    requestCases.getRequestCases().stream()
      .filter( rc -> !rc.isFailure())
      .forEach( rc -> expected.add( expectedConfig( rc)));
    
    return expected.build();
    }

  /**
   * Returns the JSON object that represents expectations for the given request case.
   */
  private JsonObject expectedConfig( RequestCase requestCase)
    {
    JsonObjectBuilder expected = Json.createObjectBuilder();

    expected.add( "request", expectedRequest( requestCase));
    expected.add( "response", expectedResponse( requestCase));

    return expected.build();
    }

  /**
   * Returns the JSON object that represents request expectations for the given request case.
   */
  private JsonObject expectedRequest( RequestCase requestCase)
    {
    JsonObjectBuilder expected = Json.createObjectBuilder();

    expected.add( "uri", expectedUri( requestCase));
    expected.add( "method", requestCase.getOperation());
    expectedQueries( requestCase).ifPresent( queries -> expected.add( "queries", queries));
    expectedHeaders( requestCase).ifPresent( headers -> expected.add( "headers", headers));
    expectedCookies( requestCase).ifPresent( cookies -> expected.add( "cookies", cookies));

    return expected.build();
    }

  /**
   * Returns the expected URI for the given request case.
   */
  private String expectedUri( RequestCase requestCase)
    {
    Matcher paramMatcher = pathParam_.matcher( requestCase.getPath());
    StringBuffer uri = new StringBuffer();
    while( paramMatcher.find())
      {
      String paramName = paramMatcher.group(1);
      
      String paramValue =
        toStream( requestCase.getParams())
        .filter( param -> param.getLocation() == PATH)
        .filter( param -> param.getName().equals( paramName))
        .map( TestWriterUtils::getPathParameterValue)
        .findFirst()
        .orElseThrow( () -> new RequestCaseException( String.format( "%s: no path parameter named '%s' found", requestCase, paramName)));
      
      paramMatcher.appendReplacement( uri, paramValue);
      }
    paramMatcher.appendTail( uri);

    return uri.toString();
    }

  /**
   * Returns the JSON object that represents the expected query parameters for the given request case.
   */
  private Optional<JsonObject> expectedQueries( RequestCase requestCase)
    {
    JsonObjectBuilder queries = Json.createObjectBuilder();

    toStream( requestCase.getParams())
      .filter( param -> param.getLocation() == QUERY)
      .flatMap( param -> TestWriterUtils.getQueryParameters( param).stream())
      .forEach( entry -> queries.add( entry.getKey(), entry.getValue()));

    return Optional.of( queries.build()).filter( json -> !json.isEmpty());
    }

  /**
   * Returns the JSON object that represents the expected cookie parameters for the given request case.
   */
  private Optional<JsonObject> expectedCookies( RequestCase requestCase)
    {
    JsonObjectBuilder cookies = Json.createObjectBuilder();

    toStream( requestCase.getParams())
      .filter( param -> param.getLocation() == COOKIE)
      .flatMap( param -> TestWriterUtils.getCookieParameters( param).stream())
      .forEach( entry -> cookies.add( entry.getKey(), entry.getValue()));

    return Optional.of( cookies.build()).filter( json -> !json.isEmpty());
    }

  /**
   * Returns the JSON object that represents the expected header parameters for the given request case.
   */
  private Optional<JsonObject> expectedHeaders( RequestCase requestCase)
    {
    JsonObjectBuilder headers = Json.createObjectBuilder();

    toStream( requestCase.getParams())
      .filter( param -> param.getLocation() == HEADER)
      .forEach( param -> TestWriterUtils.getHeaderParameterValue( param).ifPresent( value -> headers.add( param.getName(), value)));

    return Optional.of( headers.build()).filter( json -> !json.isEmpty());
    }

  /**
   * Returns the JSON object that represents response expectations for the given request case.
   */
  private JsonObject expectedResponse( RequestCase requestCase)
    {
    JsonObjectBuilder expected = Json.createObjectBuilder();

    expected.add( "status", 200);

    return expected.build();
    }

  /**
   * Returns a Writer for the given output stream;
   */
  private static Writer writerFor( OutputStream stream)
    {
    try
      {
      return
        stream == null
        ? null
        : new OutputStreamWriter( stream, "UTF-8");
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create writer", e);
      }
    }

  private Writer writer_;

  private static final Pattern pathParam_ = Pattern.compile( "\\{([^}]+)\\}");
  }
