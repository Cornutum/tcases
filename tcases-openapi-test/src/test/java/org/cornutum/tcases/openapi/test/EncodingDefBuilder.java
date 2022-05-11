//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.ArrayList;
import java.util.List;

/**
 * Builds an {@link EncodingDef} instance.
 */
public class EncodingDefBuilder
  {
  private EncodingDefBuilder( boolean multipart)
    {
    multipart_ = multipart;
    }

  /**
   * Returns a builder for a simple value encoding.
   */
  public static EncodingDefBuilder simple()
    {
    return new EncodingDefBuilder( false).style( "simple");
    }

  /**
   * Returns a builder for an <CODE>application/x-www-form-urlencoded</CODE> encoding.
   */
  public static EncodingDefBuilder urlencoded()
    {
    return new EncodingDefBuilder( false);
    }

  /**
   * Returns a builder for a <CODE>multipart/form-data</CODE> encoding.
   */
  public static EncodingDefBuilder multipart()
    {
    return new EncodingDefBuilder( true);
    }

  public EncodingDefBuilder style( String style)
    {
    if( multipart_)
      {
      throw new IllegalArgumentException( "'style' not allowed for multipart/form-data encoding");
      }

    style_ = style;
    return this;
    }

  public EncodingDefBuilder exploded()
    {
    return exploded( true);
    }

  public EncodingDefBuilder exploded( boolean exploded)
    {
    if( multipart_)
      {
      throw new IllegalArgumentException( "'exploded' not allowed for multipart/form-data encoding");
      }
    
    exploded_ = exploded;
    return this;
    }

  public EncodingDefBuilder contentType( String contentType)
    {
    if( !multipart_)
      {
      throw new IllegalArgumentException( "'contentType' not allowed for application/x-www-form-urlcoded encoding");
      }

    contentType_ = contentType;
    return this;
    }

  public EncodingDefBuilder header( HeaderDef header)
    {
    if( !multipart_)
      {
      throw new IllegalArgumentException( "'header' not allowed for application/x-www-form-urlcoded encoding");
      }

    headers_.add( header);
    return this;
    }

  public EncodingDef build()
    {
    return
      multipart_
      ? EncodingDef.forMultipartForm( contentType_, headers_)
      : EncodingDef.forUrlEncodedForm( style_, exploded_);
    }

  private final boolean multipart_;
  private String style_;
  private Boolean exploded_;
  private String contentType_;
  private List<HeaderDef> headers_ = new ArrayList<HeaderDef>();
  }
