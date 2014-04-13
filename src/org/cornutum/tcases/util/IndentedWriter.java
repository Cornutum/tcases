//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import org.apache.commons.lang3.StringUtils;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;

/**
 * Prints indented lines.
 *
 * @version $Revision$, $Date$
 */
public class IndentedWriter
  {
  /**
   * Creates a new IndentedWriter object.
   */
  public IndentedWriter()
    {
    level_  = 0;
    setIndent( 2);
    }

  /**
   * Creates a new IndentedWriter object.
   */
  public IndentedWriter( OutputStream output)
    {
    this();
    writer_ = new PrintWriter( output==null? System.out : output);
    }

  /**
   * Creates a new IndentedWriter object.
   */
  public IndentedWriter( Writer writer)
    {
    this();
    writer_ = new PrintWriter( writer);
    }

  /**
   * Prints an indented line.
   */
  public void println( String text)
    {
    startLine();
    writer_.println( text);
    }

  /**
   * Prints an empty line.
   */
  public void println()
    {
    writer_.println();
    }

  /**
   * Indents to the beginning a new line.
   */
  public void startLine()
    {
    for( int i = 0; i < level_; i++)
      {
      writer_.print( indent_);
      }
    }

  /**
   * Prints the given text.
   */
  public void print( String text)
    {
    writer_.print( text);
    }

  /**
   * Increments the indentation level.
   */
  public void indent()
    {
    level_++;
    }

  /**
   * Decrements the indentation level.
   */
  public void unindent()
    {
    if( level_ > 0)
      {
      level_--;
      }
    }

  /**
   * Flushes the writer.
   */
  public void flush()
    {
    writer_.flush();
    }

  /**
   * Closes the writer.
   */
  public void close()
    {
    writer_.close();
    }
    

  /**
   * Changes the indentation width.
   */
  public void setIndent( int indentWidth)
    {
    indent_ = StringUtils.leftPad( "", indentWidth);
    }

  /**
   * Returns the indentation width.
   */
  public int getIndent()
    {
    return indent_.length();
    }

  private int         level_;
  private String      indent_;
  private PrintWriter writer_; 
  }
