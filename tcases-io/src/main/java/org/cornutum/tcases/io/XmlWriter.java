//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.text.translate.NumericEntityEscaper;

import java.io.OutputStream;
import java.io.Writer;
import java.util.HashMap;
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

  /**
   * Creates a new ElementWriter with the given tag.
   */
  public ElementWriter element( String tag)
    {
    return new ElementWriter().tag( tag);
    }

  public class ElementWriter
    {
    public ElementWriter()
      {
      tag( null);
      attributes( new HashMap<String,String>());
      content( (String) null);
      }

    public ElementWriter tag( String tag)
      {
      tag_ = tag;
      return this;
      }

    public ElementWriter attributes( Map<String,String> attributes)
      {
      attributes_ = attributes;
      return this;
      }

    public ElementWriter attribute( String name, String value)
      {
      attributes_.put( name, value);
      return this;
      }

    public ElementWriter attributeIf( boolean condition, String name, String value)
      {
      if( condition)
        {
        attributes_.put( name, value);
        }
      return this;
      }

    public ElementWriter attributeIf( String name, Optional<String> value)
      {
      if( value.isPresent())
        {
        attributes_.put( name, value.get());
        }
      return this;
      }

    public ElementWriter content( String content)
      {
      content_ = content;
      contentWriter_ = null;
      return this;
      }

    public ElementWriter content( Runnable contentWriter)
      {
      content_ = null;
      contentWriter_ = contentWriter;
      return this;
      }

    public void write()
      {
      if( content_ != null)
        {
        startLine();
        print( "<"); print( tag_); print( ">");
        print( content_);
        print( "</"); print( tag_); print( ">");
        println();
        }
      else
        {
        writeTagStart( tag_);
        attributes_.forEach( (k,v) -> writeAttribute( k, v));

        if( contentWriter_ != null)
          {
          writeTagEnd();
          indent();
          contentWriter_.run();
          unindent();
          writeElementEnd( tag_);
          }
        else
          {
          writeEmptyElementEnd();
          }
        }
      }
    
    private String tag_;
    private Map<String,String> attributes_;
    private String content_;
    private Runnable contentWriter_;
    } 
  }
