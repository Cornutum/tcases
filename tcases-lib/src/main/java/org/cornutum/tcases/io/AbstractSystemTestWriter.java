//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

/**
 * Base class for writing a {@link SystemTestDef} to an output stream.
 *
 */
public abstract class AbstractSystemTestWriter implements Closeable
  {
  /**
   * Creates a new AbstractSystemTestWriter object that writes to standard output.
   */
  public AbstractSystemTestWriter()
    {
    this( System.out);
    }
  
  /**
   * Creates a new AbstractSystemTestWriter object that writes to the given stream.
   */
  public AbstractSystemTestWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new AbstractSystemTestWriter object that writes to the given stream.
   */
  public AbstractSystemTestWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given system test definition.
   */
  public abstract void write( SystemTestDef systemTest);

  /**
   * Flushes the writer.
   */
  public void flush() throws IOException
    {
    getWriter().flush();
    }

  /**
   * Closes the writer.
   */
  public void close() throws IOException
    {
    getWriter().close();
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
