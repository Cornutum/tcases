//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.JsonUtils.expectObject;
import static org.cornutum.tcases.openapi.test.JsonUtils.readJson;

import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.LinkedHashMap;
import java.util.Map;

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

  public ContentDefBuilder schema( String schema)
    {
    try
      {
      return schema( expectObject( readJson( schema)));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read schema", e);
      }
    }

  public ContentDefBuilder encodeValue( EncodingDef valueEncoding)
    {
    valueEncoding_ = valueEncoding;
    return this;
    }

  public ContentDefBuilder encodeValue( String style, boolean exploded)
    {
    return encodeValue( EncodingDefBuilder.urlencoded().style( style).exploded( exploded).build());
    }

  public ContentDefBuilder encodeValue( String style)
    {
    return encodeValue( EncodingDefBuilder.urlencoded().style( style).build());
    }

  public ContentDefBuilder encodeValue( boolean exploded)
    {
    return encodeValue( "simple", exploded);
    }

  public ContentDefBuilder encodeProperty( String property, EncodingDef encoding)
    {
    propertyEncodings_.put( property, encoding);
    return this;
    }

  public ContentDefBuilder encodeProperty( String property, String style, boolean exploded)
    {
    return encodeProperty( property, EncodingDefBuilder.urlencoded().style( style).exploded( exploded).build());
    }

  public ContentDefBuilder encodeProperty( String property, String style)
    {
    return encodeProperty( property, EncodingDefBuilder.urlencoded().style( style).build());
    }

  public ContentDefBuilder encodeProperty( String property, boolean exploded)
    {
    return encodeProperty( property, null, exploded);
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
