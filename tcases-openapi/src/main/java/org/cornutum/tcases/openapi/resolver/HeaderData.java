//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ParamDef.Location;
import org.cornutum.tcases.util.ToString;

import java.util.Objects;

/**
 * Defines a request header data object.
 */
public class HeaderData extends ParamData
  {
  /**
   * Creates a new HeaderData instance.
   */
  public HeaderData( String name, MessageData headerData)
    {
    this( name, headerData.getValue(), headerData.getMediaType(), headerData.isValid());
    }
  
  /**
   * Creates a new HeaderData instance.
   */
  public HeaderData( String name, DataValue<?> value,  String mediaType, boolean valid)
    {
    super( name, value, mediaType, valid);
    setLocation( ParamDef.Location.HEADER);
    setStyle( "simple");
    }

  /**
   * Changes the location of the header value.
   */
  @Override
  public void setLocation( Location location)
    {
    if( !Location.HEADER.equals( location))
      {
      throw new IllegalArgumentException( "Can't change header location");
      }
    }

  /**
   * Changes the serialization style of the header value.
   */
  @Override
  public void setStyle( String style)
    {
    if( !"simple".equals( style))
      {
      throw new IllegalArgumentException( "Header style must be 'simple'");
      }
    }

  @Override
  public boolean equals( Object object)
    {
    HeaderData other =
      object instanceof HeaderData
      ? (HeaderData) object
      : null;

    return
      other != null
      && Objects.equals( other.getName(), getName());
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getName());
    }
  
  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .appendSuper( super.toString())
      .toString();
    }
  }
