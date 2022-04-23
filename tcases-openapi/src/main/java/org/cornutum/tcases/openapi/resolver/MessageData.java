//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toMap;

/**
 * Defines a data object exchanged in an API request or response.
 */
public class MessageData
  {
  /**
   * Creates a new MessageData instance.
   */
  public MessageData( DataValue<?> value,  String mediaType, boolean valid)
    {
    value_ = value;
    mediaType_ = mediaType;
    valid_ = valid;
    setEncodings( null);
    }

  /**
   * Returns the data value.
   */
  public DataValue<?> getValue()
    {
    return value_;
    }

  /**
   * Returns the data value type.
   */
  public DataValue.Type getType()
    {
    return Optional.ofNullable( getValue()).map( DataValue::getType).orElse( null);
    }

  /**
   * Returns if this is a valid data value.
   */
  public boolean isValid()
    {
    return valid_;
    }

  /**
   * Returns the data media type.
   */
  public String getMediaType()
    {
    return mediaType_;
    }

  /**
   * Changes the encodings for object value properties.
   */
  public void setEncodings( Map<String,EncodingData> encodings)
    {
    encodings_ =
      Optional.ofNullable( encodings)
      .map( Map::keySet)
      .orElse( emptySet())
      .stream()
      .collect( toMap( property -> property, property -> encodings.get( property), (v1,v2) -> v1, LinkedHashMap::new));
    }

  /**
   * Returns the encodings for object value properties.
   */
  public Map<String,EncodingData> getEncodings()
    {
    return encodings_;
    }

  @Override
  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    if( !isValid())
      {
      builder.append( "FAILURE");
      }

    builder.append( getValue());

    Optional.ofNullable( getMediaType()).ifPresent( mediaType -> builder.append( mediaType));
    
    return builder.toString();
    }

  private final DataValue<?> value_;
  private final String mediaType_;
  private final boolean valid_;
  private Map<String,EncodingData> encodings_;
  }
