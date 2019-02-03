//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.io.SystemTestJson.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Arrays;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonStructure;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import static javax.json.stream.JsonGenerator.PRETTY_PRINTING;

/**
 * Writes a {@link SystemTestDef} in the form of a JSON document.
 *
 */
public class SystemTestJsonWriter implements Closeable
  {
  /**
   * Creates a new SystemTestJsonWriter object that writes to standard output.
   */
  public SystemTestJsonWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new SystemTestJsonWriter object that writes to the given stream.
   */
  public SystemTestJsonWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new SystemTestJsonWriter object that writes to the given stream.
   */
  public SystemTestJsonWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given system test definition the form of a JSON document.
   */
  public void write( SystemTestDef systemTest)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( toJson( systemTest));
    }

  /**
   * Returns the JSON object that represents the given system test definition.
   */
  private JsonStructure toJson( SystemTestDef systemTest)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    builder.add( SYSTEM_KEY, systemTest.getName());

    addAnnotations( builder, systemTest);
    
    toStream( systemTest.getFunctionTestDefs()).forEach( function -> builder.add( function.getName(), toJson( function)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given function test definition.
   */
  private JsonStructure toJson( FunctionTestDef functionTest)
    {
    JsonArrayBuilder builder = Json.createArrayBuilder();

    toStream( functionTest.getTestCases()).forEach( testCase -> builder.add( toJson( testCase)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given test case.
   */
  private JsonStructure toJson( TestCase testCase)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( ID_KEY, testCase.getId());
    addAnnotations( builder, testCase);

    Arrays.stream( testCase.getVarTypes()).forEach( varType -> builder.add( varType, toJson( testCase, varType)));
    
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the bindings for variables of the given type.
   */
  private JsonStructure toJson( TestCase testCase, String varType)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    toStream( testCase.getVarBindings( varType))
      .sorted()
      .forEach( binding -> builder.add( binding.getVar(), toJson( binding)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given variable binding.
   */
  private JsonStructure toJson( VarBinding binding)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, binding);
    
    if( binding.isValueNA())
      {
      builder.add( NA_KEY, true);
      }
    else
      {
      if( !binding.isValueValid())
        {
        builder.add( FAILURE_KEY, true);
        }
      builder.add( VALUE_KEY, String.valueOf( binding.getValue()));
      }

    return builder.build();
    }

  /**
   * Add any annotatations from the given Annotated object to the given JsonObjectBuilder.
   */
  private JsonObjectBuilder addAnnotations( JsonObjectBuilder builder, IAnnotated annotated)
    {
    JsonObjectBuilder annotations = Json.createObjectBuilder();
    toStream( annotated.getAnnotations()).forEach( name -> annotations.add( name, annotated.getAnnotation( name)));
    JsonObject json = annotations.build();

    if( !json.isEmpty())
      {
      builder.add( HAS_KEY, json);
      }

    return builder;
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
  }
