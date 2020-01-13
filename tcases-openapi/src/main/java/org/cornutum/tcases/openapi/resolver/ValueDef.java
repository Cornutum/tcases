//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

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
   * Changes the format of this value.
   */
  public void setFormat( String format)
    {
    format_ = format;
    }

  /**
   * Returns the format of this value.
   */
  public String getFormat()
    {
    return format_;
    }
  
  /**
   * Changes the media type for this value.
   */
  public void setMediaType( String mediaType)
    {
    mediaType_ = mediaType;
    }

  /**
   * Returns the media type for this value.
   */
  public String getMediaType()
    {
    return mediaType_;
    }
  
  /**
   * Changes the media types NOT allowed for this value.
   */
  public void setNotMediaTypes( Iterable<String> mediaTypes)
    {
    notMediaTypes_ = mediaTypes;
    }

  /**
   * Returns the media types NOT allowed for this value.
   */
  public Iterable<String> getNotMediaTypes()
    {
    return notMediaTypes_;
    }

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
  private String format_;
  private String mediaType_;
  private Iterable<String> notMediaTypes_;
  }
