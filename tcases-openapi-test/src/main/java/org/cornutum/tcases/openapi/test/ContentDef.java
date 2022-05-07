//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Map;
import java.util.Optional;

/**
 * Defines the form of response content
 */
public class ContentDef
  {
  /**
   * Creates a new ContentDef instance.
   */
  public ContentDef( String contentType, ObjectNode schema, Boolean exploded, Map<String,EncodingDef> encodings)
    {
    contentType_ = contentType;
    schema_ = schema;
    exploded_ = Optional.ofNullable( exploded).orElse( false);
    encodings_ = encodings;
    }

  /**
   * Returns the content type.
   */
  public String getContentType()
    {
    return contentType_;
    }

  /**
   * Returns the content schema.
   */
  public ObjectNode getSchema()
    {
    return schema_;
    }

  /**
   * Returns if the content is serialized in exploded form.
   */
  public boolean isExploded()
    {
    return exploded_;
    }

  /**
   * Returns the content encodings.
   */
  public Map<String,EncodingDef> getEncodings()
    {
    return encodings_;
    }

  @Override
  public String toString()
    {
    return
      ToString.builder( getClass())
      .add( "contentType", getContentType())
      .addIf( "dataType", Optional.ofNullable( getSchema()).flatMap( s -> Optional.ofNullable( s.get( "type").asText())))
      .addIf( "exploded=", Optional.of( isExploded()).filter( e -> e))
      .toString();
    }
  
  private final String contentType_;
  private final ObjectNode schema_;
  private final boolean exploded_;
  private final Map<String,EncodingDef> encodings_;
  }
