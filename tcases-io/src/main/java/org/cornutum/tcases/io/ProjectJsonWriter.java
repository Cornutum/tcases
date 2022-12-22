//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.util.MapBuilder;
import org.slf4j.LoggerFactory;

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
 * Writes a {@link Project} in the form of a JSON document.
 *
 */
public class ProjectJsonWriter implements Closeable
  {
  /**
   * Creates a new ProjectJsonWriter object that writes to standard output.
   */
  public ProjectJsonWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new ProjectJsonWriter object that writes to the given stream.
   */
  public ProjectJsonWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new ProjectJsonWriter object that writes to the given stream.
   */
  public ProjectJsonWriter( Writer writer)
    {
    converter_ = new ProjectJson( LoggerFactory.getLogger( ProjectJsonWriter.class));
    setWriter( writer);
    }

  /**
   * Writes the given project definition the form of a JSON document.
   */
  public void write( Project project)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( converter_.toJson( project));
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

  private final ProjectJson converter_;
  private Writer writer_;  
  }
