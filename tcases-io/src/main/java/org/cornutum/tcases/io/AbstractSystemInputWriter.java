//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

/**
 * Base class for writing a {@link SystemInputDef} to an output stream.
 *
 */
public abstract class AbstractSystemInputWriter implements Closeable
  {
  /**
   * Creates a new AbstractSystemInputWriter object that writes to standard output.
   */
  public AbstractSystemInputWriter()
    {
    this( System.out);
    }
  
  /**
   * Creates a new AbstractSystemInputWriter object that writes to the given stream.
   */
  public AbstractSystemInputWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new AbstractSystemInputWriter object that writes to the given stream.
   */
  public AbstractSystemInputWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given system test definition.
   */
  public abstract void write( SystemInputDef systemInput);

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
  @Override
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
