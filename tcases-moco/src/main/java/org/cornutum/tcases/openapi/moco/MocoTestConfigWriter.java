//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.apache.commons.io.IOUtils;
import org.cornutum.tcases.util.MapBuilder;

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
 * Writes a  {@link MocoTestConfig} object in the form of a JSON document.
 *
 */
public class MocoTestConfigWriter implements Closeable
  {
  /**
   * Creates a new MocoTestConfigWriter object that writes to standard output.
   */
  public MocoTestConfigWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new MocoTestConfigWriter object that writes to the given stream.
   */
  public MocoTestConfigWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new MocoTestConfigWriter object that writes to the given stream.
   */
  public MocoTestConfigWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given request cases in the form of a JSON document.
   */
  public void write( MocoTestConfig mocoTestConfig)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( MocoTestConfigJson.toJson( mocoTestConfig));
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
