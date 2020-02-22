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
  private ValueDomain<String> mediaType_;
  }
