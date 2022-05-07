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

  public ContentDefBuilder exploded()
    {
    return exploded( true);
    }

  public ContentDefBuilder exploded( Boolean exploded)
    {
    exploded_ = exploded;
    return this;
    }

  public ContentDefBuilder encode( String property, EncodingDef encoding)
    {
    encodings_.put( property, encoding);
    return this;
    }

  public ContentDef build()
    {
    return new ContentDef( contentType_, schema_, exploded_, encodings_);
    }

  private String contentType_;
  private ObjectNode schema_;
  private Boolean exploded_;
  private Map<String,EncodingDef> encodings_ = new LinkedHashMap<String,EncodingDef>();
  }
