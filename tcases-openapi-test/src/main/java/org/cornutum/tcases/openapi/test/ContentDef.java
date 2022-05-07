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
  public ContentDef( String contentType, ObjectNode schema, EncodingDef valueEncoding, Map<String,EncodingDef> propertyEncodings)
    {
    contentType_ = contentType;
    schema_ = schema;
    valueEncoding_ = Optional.ofNullable( valueEncoding).orElse( EncodingDef.forSimpleValue( false));
    propertyEncodings_ = propertyEncodings;
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
    return getValueEncoding().isExploded();
    }

  /**
   * Returns the content value encoding.
   */
  public EncodingDef getValueEncoding()
    {
    return valueEncoding_;
    }

  /**
   * Returns the content object property encodings.
   */
  public Map<String,EncodingDef> getPropertyEncodings()
    {
    return propertyEncodings_;
    }

  @Override
  public String toString()
    {
    return
      ToString.builder( getClass())
      .add( "contentType", getContentType())
      .addIf( "dataType", Optional.ofNullable( getSchema()).flatMap( s -> Optional.ofNullable( s.get( "type").asText())))
      .add( "valueEncoding=", getValueEncoding())
      .toString();
    }
  
  private final String contentType_;
  private final ObjectNode schema_;
  private final EncodingDef valueEncoding_;
  private final Map<String,EncodingDef> propertyEncodings_;
  }
