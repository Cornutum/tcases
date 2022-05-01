//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.ArrayList;
import java.util.List;

/**
 * Builds an {@link EncodingData} instance.
 */
public class EncodingDataBuilder
  {
  private EncodingDataBuilder( boolean multipart)
    {
    multipart_ = multipart;
    }

  /**
   * Returns a builder for an <CODE>application/x-www-form-urlencoded</CODE> encoding.
   */
  public static EncodingDataBuilder urlencoded()
    {
    return new EncodingDataBuilder( false);
    }

  /**
   * Returns a builder for a <CODE>multipart/form-data</CODE> encoding.
   */
  public static EncodingDataBuilder multipart()
    {
    return new EncodingDataBuilder( true);
    }

  public EncodingDataBuilder style( String style)
    {
    if( multipart_)
      {
      throw new IllegalArgumentException( "'style' not allowed for multipart/form-data encoding");
      }

    style_ = style;
    return this;
    }

  public EncodingDataBuilder exploded()
    {
    return exploded( true);
    }

  public EncodingDataBuilder exploded( boolean exploded)
    {
    if( multipart_)
      {
      throw new IllegalArgumentException( "'exploded' not allowed for multipart/form-data encoding");
      }
    
    exploded_ = exploded;
    return this;
    }

  public EncodingDataBuilder contentType( String contentType)
    {
    if( !multipart_)
      {
      throw new IllegalArgumentException( "'contentType' not allowed for application/x-www-form-urlcoded encoding");
      }

    contentType_ = contentType;
    return this;
    }

  public EncodingDataBuilder header( HeaderData header)
    {
    if( !multipart_)
      {
      throw new IllegalArgumentException( "'header' not allowed for application/x-www-form-urlcoded encoding");
      }

    headers_.add( header);
    return this;
    }

  public EncodingData build()
    {
    return
      multipart_
      ? new EncodingData( contentType_, headers_)
      : new EncodingData( style_, exploded_);
    }

  private final boolean multipart_;
  private String style_;
  private boolean exploded_ = true;
  private String contentType_;
  private List<HeaderData> headers_ = new ArrayList<HeaderData>();
  }
