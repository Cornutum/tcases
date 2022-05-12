//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.List;
import java.util.Optional;
import static java.util.Collections.emptyList;

/**
 * Represents the encoding used to serialize part of a response object.
 */
public class EncodingDef
  {
  /**
   * Returns an {@link EncodingDef} for a "simple" style value.
   */
  public static EncodingDef forSimpleValue( Boolean exploded)
    {
    return forSimpleValue( "simple", exploded);
    }
  
  /**
   * Returns an {@link EncodingDef} for a "simple" style value.
   */
  public static EncodingDef forSimpleValue( String style, Boolean exploded)
    {
    return new EncodingDef( style, Optional.ofNullable( exploded).orElse( false), null, null);
    }
  
  /**
   * Returns an {@link EncodingDef} for an <CODE>application/x-www-form-urlencoded</CODE> part.
   */
  public static EncodingDef forUrlEncodedForm( String style, Boolean exploded)
    {
    String styleApplied = Optional.ofNullable( style).orElse( "form");
    boolean explodedApplied = Optional.ofNullable( exploded).orElse( "form".equals( styleApplied));
    return new EncodingDef( styleApplied, explodedApplied, null, null);
    }
  
  /**
   * Returns a default {@link EncodingDef} for an <CODE>application/x-www-form-urlencoded</CODE> part.
   */
  public static EncodingDef forUrlEncodedForm()
    {
    return forUrlEncodedForm( null, null);
    }

  /**
   * Returns an {@link EncodingDef} for a <CODE>multipart/form-data</CODE> part.
   */
  public static EncodingDef forMultipartForm( String contentType, List<HeaderDef> headers)
    {
    return new EncodingDef( null, null, contentType, headers);
    }

  /**
   * Creates a new EncodingDef instance.
   */
  private EncodingDef( String style, Boolean exploded, String contentType, List<HeaderDef> headers)
    {
    style_ = style;
    exploded_ = exploded;
    contentType_ = contentType;
    headers_ = Optional.ofNullable( headers).orElse( emptyList());
    }

  /**
   * Returns the serialization style of an <CODE>application/x-www-form-urlencoded</CODE> part.
   */
  public String getStyle()
    {
    return style_;
    }

  /**
   * Returns if an <CODE>application/x-www-form-urlencoded</CODE> part uses "exploded" serialization.
   */
  public Boolean isExploded()
    {
    return exploded_;
    }

  /**
   * Returns the content type of a <CODE>multipart/form-data</CODE> part.
   */
  public String getContentType()
    {
    return contentType_;
    }

  /**
   * Returns definitions of headers for a <CODE>multipart/form-data</CODE> part.
   */
  public List<HeaderDef> getHeaders()
    {
    return headers_;
    }

  @Override
  public String toString()
    {
    return
      ToString.builder( getClass())
      .addIf( "style", Optional.ofNullable( getStyle()))
      .addIf( "exploded", Optional.ofNullable( isExploded()))
      .addIf( "contentType", Optional.ofNullable( getContentType()))
      .toString();
    }

  private final String style_;
  private final Boolean exploded_;
  private final String contentType_;
  private final List<HeaderDef> headers_;
  }
