//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.io.OutputStream;
import java.io.Writer;
  
/**
 * Supports creation of an XML document stream.
 *
 */
public class XmlWriter extends IndentedWriter
  {
  /**
   * Creates a new XmlWriter object.
   */
  public XmlWriter( OutputStream output)
    {
    super( output);
    }

  /**
   * Creates a new XmlWriter object.
   */
  public XmlWriter( Writer writer)
    {
    super( writer);
    }

  /**
   * Writes the standard XML document declaration
   */
  public void writeDeclaration( String encoding)
    {
    print( "<?xml version=\"1.0\"");
    if( encoding != null)
      {
      writeAttribute( "encoding", encoding);
      }
    println( "?>");
    }

  /**
   * Writes the standard XML document declaration
   */
  public void writeDeclaration()
    {
    writeDeclaration( null);
    }

  /**
   * Begins an element start tag.
   */
  public void writeTagStart( String tag)
    {
    startLine();
    print( "<");
    print( tag);
    }

  /**
   * Completes an element start tag.
   */
  public void writeTagEnd()
    {
    print( ">");
    println();
    }

  /**
   * Writes an element on a single line.
   */
  public void writeElement( String tag, String content)
    {
    startLine();
    print( "<"); print( tag); print( ">");
    print( content);
    print( "</"); print( tag); print( ">");
    println();
    }

  /**
   * Writes an element start tag.
   */
  public void writeElementStart( String tag)
    {
    startLine();
    print( "<");
    print( tag);
    print( ">");
    println();
    }

  /**
   * Writes an element end tag.
   */
  public void writeElementEnd( String tag)
    {
    startLine();
    print( "</");
    print( tag);
    print( ">");
    println();
    }

  /**
   * Writes the end of an empty element.
   */
  public void writeEmptyElementEnd()
    {
    print( "/>");
    println();
    }

  /**
   * Writes an attribute definition.
   */
  public void writeAttribute( String name, String value)
    {
    print( " ");
    print( name);
    print( "=\"");
    print( value);
    print( "\"");
    }
  }
