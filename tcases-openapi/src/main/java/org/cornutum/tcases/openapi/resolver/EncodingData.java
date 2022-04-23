//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Optional;
import static java.util.Collections.emptyList;

/**
 * Represents the encoding used to serialize part of an {@link ObjectValue}.
 */
public class EncodingData
  {
  /**
   * Creates a new EncodingData instance.
   */
  public EncodingData( String style, Boolean exploded)
    {
    this( style, exploded, null, null);
    }
  
  /**
   * Creates a new EncodingData instance.
   */
  public EncodingData( String contentType, List<HeaderData> headers)
    {
    this( null, null, contentType, headers);
    }
  
  /**
   * Creates a new EncodingData instance.
   */
  private EncodingData( String style, Boolean exploded, String contentType, List<HeaderData> headers)
    {
    style_ = style;
    exploded_ = exploded;
    contentType_ = contentType;
    headers_ = headers;
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
  public List<HeaderData> getHeaders()
    {
    return headers_;
    }

  /**
   * Changes the definitions of headers for a <CODE>multipart/form-data</CODE> part.
   */
  public void setHeaders( List<HeaderData> headers)
    {
    headers_ = Optional.ofNullable( headers).orElse( emptyList());
    }

  @Override
  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    Optional.ofNullable( getStyle())
      .ifPresent( style -> builder.append( "style", style));
    Optional.ofNullable( isExploded())
      .ifPresent( exploded -> builder.append( "exploded", exploded));
    Optional.ofNullable( getContentType())
      .ifPresent( contentType -> builder.append( "contentType", contentType));
    
    return builder.toString();
    }

  private final String style_;
  private final Boolean exploded_;
  private final String contentType_;
  private List<HeaderData> headers_;
  }
