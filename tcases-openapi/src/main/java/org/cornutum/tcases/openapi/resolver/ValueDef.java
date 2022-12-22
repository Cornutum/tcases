//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.resolve.ValueDomain;
import org.cornutum.tcases.util.ToString;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toMap;

/**
 * Defines a request input value.
 */
public class ValueDef<T>
  {
  /**
   * Creates a new ValueDef instance.
   */
  public ValueDef( ValueDomain<T> domain)
    {
    domain_ = domain;
    setValid( true);
    setEncodings( null);
    }

  /**
   * Returns the domain for this value.
   */
  public ValueDomain<T> getDomain()
    {
    return domain_;
    }

  /**
   * Returns true if this input value is defined.
   */
  public boolean isDefined()
    {
    return getDomain() != null;
    }

  /**
   * Changes if this is an instance of valid value.
   */
  public void setValid( boolean valid)
    {
    valid_ = valid;
    }

  /**
   * Returns if this is an instance of valid value.
   */
  public boolean isValid()
    {
    return valid_;
    }
  
  /**
   * Changes the media type domain for this value.
   */
  public void setMediaType( ValueDomain<String> mediaType)
    {
    mediaType_ = mediaType;
    }

  /**
   * Returns the media type domain for this value.
   */
  public ValueDomain<String> getMediaType()
    {
    return mediaType_;
    }

  /**
   * Changes the encodings for object value properties.
   */
  public void setEncodings( Map<String,EncodingDef> encodings)
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
  public Map<String,EncodingDef> getEncodings()
    {
    return encodings_;
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( isDefined()? String.valueOf( getDomain()) : "Undefined")
      .append( isValid()? "SUCCESS" : "FAILURE")
      .toString();
    }

  private final ValueDomain<T> domain_;
  private boolean valid_;
  private ValueDomain<String> mediaType_;
  private Map<String,EncodingDef> encodings_;
  }
