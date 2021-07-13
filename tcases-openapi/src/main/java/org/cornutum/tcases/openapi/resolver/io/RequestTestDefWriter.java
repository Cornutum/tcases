//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver.io;

import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.util.MapBuilder;

import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import javax.json.Json;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import static javax.json.stream.JsonGenerator.PRETTY_PRINTING;

/**
 * Writes a  {@link RequestTestDef} object in the form of a JSON document.
 *
 */
public class RequestTestDefWriter implements Closeable
  {
  /**
   * Creates a new RequestTestDefWriter object that writes to standard output.
   */
  public RequestTestDefWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new RequestTestDefWriter object that writes to the given stream.
   */
  public RequestTestDefWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new RequestTestDefWriter object that writes to the given stream.
   */
  public RequestTestDefWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given request cases in the form of a JSON document.
   */
  public void write( RequestTestDef requestTestDef)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( RequestCaseJson.toJson( requestTestDef));
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
  @Override
public void close()
    {
    IOUtils.closeQuietly( getWriter(), null);
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
