//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Optional;

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
  }
