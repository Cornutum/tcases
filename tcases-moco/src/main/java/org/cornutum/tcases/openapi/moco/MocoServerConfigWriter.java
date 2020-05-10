////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.openapi.resolver.ArrayValue;
import org.cornutum.tcases.openapi.resolver.Base64Domain;
import org.cornutum.tcases.openapi.resolver.BinaryValue;
import org.cornutum.tcases.openapi.resolver.BooleanValue;
import org.cornutum.tcases.openapi.resolver.DataValue;
import org.cornutum.tcases.openapi.resolver.DataValueVisitor;
import org.cornutum.tcases.openapi.resolver.DecimalValue;
import org.cornutum.tcases.openapi.resolver.IntegerValue;
import org.cornutum.tcases.openapi.resolver.LongValue;
import org.cornutum.tcases.openapi.resolver.NullValue;
import org.cornutum.tcases.openapi.resolver.ObjectValue;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestCaseException;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.resolver.StringValue;
import org.cornutum.tcases.openapi.resolver.io.DataValueText;
import org.cornutum.tcases.openapi.resolver.io.FormUrlEncoder;
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
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import static java.util.Collections.emptyList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
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

    try
      {
      jsonWriter.write( expectedConfigs( requestCases));
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't write Moco server configuration for %s", requestCases), e);
      }
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
    try
      {
      JsonObjectBuilder expected = Json.createObjectBuilder();

      expected.add( "request", expectedRequest( requestCase));
      expected.add( "response", expectedResponse( requestCase));

      return expected.build();
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't write Moco server configuration for %s", requestCase), e);
      }
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
    expectedEmptyBody( requestCase).ifPresent( emptyBody -> expected.add( "text", emptyBody));
    expectedJsonBody( requestCase).ifPresent( jsonBody -> expected.add( "json_paths", jsonBody));
    expectedFormBody( requestCase).ifPresent( formBody -> expected.add( "forms", formBody));
    expectedTextBody( requestCase).ifPresent( textBody -> expected.add( "text", textBody));

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
   * Returns the JSON object that represents the expected JSON request body for the given request case.
   */
  private Optional<JsonObject> expectedJsonBody( RequestCase requestCase)
    {
    JsonObjectBuilder jsonBody = Json.createObjectBuilder();

    Optional.ofNullable( requestCase.getBody())
      .filter( body -> "application/json".equals( body.getMediaType()))
      .map( body -> DataValueJsonPath.expected( body.getValue()))
      .orElse( emptyList())
      .stream().forEach( entry -> jsonBody.add( entry.getKey(), entry.getValue()));

    return Optional.of( jsonBody.build()).filter( json -> !json.isEmpty());
    }

  /**
   * Returns the JSON object that represents the expected form request body for the given request case.
   */
  private Optional<JsonObject> expectedFormBody( RequestCase requestCase)
    {
    JsonObjectBuilder formBody = Json.createObjectBuilder();

    Optional.ofNullable( requestCase.getBody())
      .filter( body -> "application/x-www-form-urlencoded".equals( body.getMediaType()))
      .map( body -> FormUrlEncoder.encode( body.getValue(), false))
      .orElse( emptyList())
      .stream().forEach( entry -> formBody.add( entry.getKey(), entry.getValue()));

    return Optional.of( formBody.build()).filter( json -> !json.isEmpty());
    }

  /**
   * Returns the JSON object that represents the expected text request body for the given request case.
   */
  private Optional<JsonValue> expectedTextBody( RequestCase requestCase)
    {
    JsonObjectBuilder textBody = Json.createObjectBuilder();

    Optional.ofNullable( requestCase.getBody())
      .filter( body -> body.getValue() != null)
      .filter( body -> body.getMediaType().startsWith( "text/"))
      .map( body -> DataValueText.toText( body.getValue()))
      .ifPresent( text -> textBody.add( "body", text));

    return Optional.ofNullable( textBody.build().get( "body"));
    }

  /**
   * Returns the JSON value that represents the expected empty request body for the given request case.
   */
  private Optional<JsonObject> expectedEmptyBody( RequestCase requestCase)
    {
    JsonObjectBuilder emptyBody = Json.createObjectBuilder();

    if( !Optional.ofNullable( requestCase.getBody()).flatMap( body -> Optional.ofNullable( body.getValue())).isPresent())
      {
      emptyBody.add( "exist", "false");
      }
    return Optional.of( emptyBody.build()).filter( json -> !json.isEmpty());
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

    Optional.ofNullable( requestCase.getBody())
      .flatMap( body -> Optional.ofNullable( body.getMediaType()))
      .ifPresent( mediaType -> {
        JsonObjectBuilder contentType = Json.createObjectBuilder();
        contentType.add( "contain", mediaType);
        headers.add( "Content-Type", contentType.build());
        });

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

  /**
   * Returns the expected JsonPath assertions for a {@link DataValue}.
   */
  private static class DataValueJsonPath implements DataValueVisitor
    {
    /**
     * Creates a new DataValueJsonPath instance.
     */
    private DataValueJsonPath( DataValue<?> value)
      {
      value_ = value;
      }

    /**
     * Returns the expected JsonPath assertions for the given {@link DataValue}.
     */
    public static List<Map.Entry<String,String>> expected( DataValue<?> value)
      {
      return new DataValueJsonPath( value).getExpected();
      }

    /**
     * Returns the expected JsonPath assertions for this {@link DataValue}.
     */
    private List<Map.Entry<String,String>> getExpected()
      {
      if( value_ != null)
        {
        value_.accept( this);
        }
      
      return expected_;
      }

    /**
     * Adds an assertion of this value for the current JSON path.
     */
    private void expect( DataValue<?> value)
      {
      expect( Objects.toString( value.getValue(), ""));
      }

    /**
     * Adds an assertion of this value for the current JSON path.
     */
    private void expect( String value)
      {
      expected_.add( new SimpleEntry<String,String>( getJsonPath(), value));
      }

    /**
     * With the given property added to the path, perform the given action.
     */
    private void forProperty( String value, Runnable action)
      {
      path_.addFirst( "." + value);
      action.run();
      }

    /**
     * With the given array member added to the path, perform the given action.
     */
    private void forMember( int i, Runnable action)
      {
      path_.addFirst( "[" + i + "]");
      action.run();
      }

    /**
     * With the given object member added to the path, perform the given action.
     */
    private void forMember( String member, Runnable action)
      {
      path_.addFirst( "['" + member + "']");
      action.run();
      }

    /**
     * Returns the current JSON path.
     */
    private String getJsonPath()
      {
      StringBuilder jsonPath = new StringBuilder().append( "$");
      String next;
      while( (next = path_.pollFirst()) != null)
        {
        jsonPath.append( next);
        }
      return jsonPath.toString();
      }
    
    public void visit( ArrayValue<?> data)
      {
      int length = data.getValue().size();
      forProperty( "length()", () -> expect( String.valueOf( length)));
      IntStream.range( 0, length).forEach( i -> forMember( i, () -> data.getValue().get(i).accept( this)));
      }

    public void visit( BinaryValue data)
      {
      expect( Base64Domain.encoded( data.getValue()));
      }

    public void visit( BooleanValue data)
      {
      expect( data);
      }

    public void visit( DecimalValue data)
      {
      expect( data);
      }

    public void visit( IntegerValue data)
      {
      expect( data);
      }

    public void visit( LongValue data)
      {
      expect( data);
      }

    public void visit( NullValue data)
      {
      expect( data);
      }

    public void visit( ObjectValue data)
      {
      data.getValue().forEach( (member,value) -> forMember( member, () -> value.accept( this)));
      }

    public void visit( StringValue data)
      {
      expect( data);
      }

    private DataValue<?> value_;
    private Deque<String> path_ = new ArrayDeque<String>();
    private List<Map.Entry<String,String>> expected_ = new ArrayList<Map.Entry<String,String>>();
    }
  }
