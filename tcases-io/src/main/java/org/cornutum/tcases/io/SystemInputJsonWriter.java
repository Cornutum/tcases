//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.util.ContextHandler;
import org.cornutum.tcases.util.MapBuilder;

import org.apache.commons.io.IOUtils;
import org.slf4j.LoggerFactory;

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
 * Writes a {@link SystemInputDef} in the form of a JSON document.
 *
 */
public class SystemInputJsonWriter extends ContextHandler<SystemInputContext> implements Closeable
  {
  /**
   * Creates a new SystemInputJsonWriter object that writes to standard output.
   */
  public SystemInputJsonWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new SystemInputJsonWriter object that writes to the given stream.
   */
  public SystemInputJsonWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new SystemInputJsonWriter object that writes to the given stream.
   */
  public SystemInputJsonWriter( Writer writer)
    {
    super( new SystemInputContext( LoggerFactory.getLogger( SystemInputJsonWriter.class)));
    converter_ = new SystemInputJson( getContext());
    setWriter( writer);
    }

  /**
   * Writes the given system test definition the form of a JSON document.
   */
  public void write( SystemInputDef systemInput)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( getConverter().toJson( systemInput));
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
    IOUtils.closeQuietly( getWriter());
    }

  /**
   * Returns the JSON converter for this writer.
   */
  private SystemInputJson getConverter()
    {
    return converter_;
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

  private final SystemInputJson converter_;
  private Writer writer_;  
  }
