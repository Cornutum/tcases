//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.LinkedHashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Builds a {@link ContentDef} instance.
 */
public class ContentDefBuilder
  {
  /**
   * Creates a new ContentDefBuilder instance.
   */
  private ContentDefBuilder()
    {
    }

  /**
   * Returns a new ContentDefBuilder instance for the given content type.
   */
  public static ContentDefBuilder forType( String contentType)
    {
    return new ContentDefBuilder().contentType( contentType);
    }

  public ContentDefBuilder contentType( String contentType)
    {
    contentType_ = contentType;
    return this;
    }

  public ContentDefBuilder schema( ObjectNode schema)
    {
    schema_ = schema;
    return this;
    }

  public ContentDefBuilder encodeValue( EncodingDef valueEncoding)
    {
    valueEncoding_ = valueEncoding;
    return this;
    }

  public ContentDefBuilder encodeProperty( String property, EncodingDef encoding)
    {
    propertyEncodings_.put( property, encoding);
    return this;
    }

  public ContentDef build()
    {
    return new ContentDef( contentType_, schema_, valueEncoding_, propertyEncodings_);
    }

  private String contentType_;
  private ObjectNode schema_;
  private EncodingDef valueEncoding_;
  private Map<String,EncodingDef> propertyEncodings_ = new LinkedHashMap<String,EncodingDef>();
  }
