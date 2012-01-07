//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.util;

import java.io.OutputStream;
import java.io.Writer;
  
/**
 * Supports creation of an XML document stream.
 *
 * @version $Revision$, $Date$
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
    print( "/>");
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
