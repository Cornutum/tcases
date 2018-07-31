//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.text.translate.NumericEntityEscaper;

import java.io.OutputStream;
import java.io.Writer;
import java.util.Map;
import java.util.Optional;
  
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
  protected void writeTagStart( String tag)
    {
    startLine();
    print( "<");
    print( tag);
    }

  /**
   * Completes an element start tag.
   */
  protected void writeTagEnd()
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
   * Writes an element with the given attributes and content.
   */
  public void writeElement( String tag, Optional<Map<String,String>> attributes, Optional<Runnable> contentWriter)
    {
    writeTagStart( tag);
    attributes.ifPresent( m -> m.forEach( (k,v) -> writeAttribute( k, v)));

    if( contentWriter.isPresent())
      {
      writeTagEnd();
      indent();
      contentWriter.get().run();
      unindent();
      writeElementEnd( tag);
      }
    else
      {
      writeEmptyElementEnd();
      }
    }

  /**
   * Writes an empty element.
   */
  public void writeElement( String tag)
    {
    writeElement( tag, Optional.empty(), Optional.empty());
    }

  /**
   * Writes an element with the given attributes.
   */
  public void writeElement( String tag, Map<String,String> attributes)
    {
    writeElement( tag, Optional.of( attributes), Optional.empty());
    }

  /**
   * Writes an element with the given content.
   */
  public void writeElement( String tag, Runnable contentWriter)
    {
    writeElement( tag, Optional.empty(), Optional.of( contentWriter));
    }

  /**
   * Writes an element with the given attributes and content.
   */
  public void writeElement( String tag, Map<String,String> attributes, Runnable contentWriter)
    {
    writeElement( tag, Optional.of( attributes), Optional.of( contentWriter));
    }

  /**
   * Writes an element end tag.
   */
  protected void writeElementEnd( String tag)
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
  protected void writeEmptyElementEnd()
    {
    print( "/>");
    println();
    }

  /**
   * Writes an attribute definition.
   */
  protected void writeAttribute( String name, String value)
    {
    print( " ");
    print( name);
    print( "=\"");
    // StringEscapeUtils escapes symbols ', < >, &, ", and some control characters
    // NumericEntityEscaper translates additional control characters \n, \t, ...
    print( NumericEntityEscaper.below(0x20).translate(StringEscapeUtils.escapeXml11(value)));
    print( "\"");
    }
  }
