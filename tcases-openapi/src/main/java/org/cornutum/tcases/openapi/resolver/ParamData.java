//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ParamDef.Location;
import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.util.ToString;

import java.util.Objects;

/**
 * Defines a request parameter data object.
 */
public class ParamData extends MessageData
  {
  /**
   * Creates a new ParamData instance.
   */
  public ParamData( String name, MessageData paramData)
    {
    this( name, paramData.getValue(), paramData.getMediaType(), paramData.isValid());
    }
  
  /**
   * Creates a new ParamData instance.
   */
  public ParamData( String name, DataValue<?> value,  String mediaType, boolean valid)
    {
    super( value, mediaType, valid);
    name_ = name;
    }

  /**
   * Returns the name of this parameter.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Changes the location of the parameter value.
   */
  public void setLocation( Location location)
    {
    location_ = location;
    }

  /**
   * Returns the location of the parameter value.
   */
  public Location getLocation()
    {
    return location_;
    }

  /**
   * Changes the serialization style of the parameter value.
   */
  public void setStyle( String style)
    {
    style_ = style;
    }

  /**
   * Returns the serialization style of the parameter value.
   */
  public String getStyle()
    {
    return style_;
    }

  /**
   * Changes if the parameter value is serialized in exploded form.
   */
  public void setExploded( boolean exploded)
    {
    exploded_ = exploded;
    }

  /**
   * Returns if the parameter value is serialized in exploded form.
   */
  public boolean isExploded()
    {
    return exploded_;
    }

  @Override
  public boolean equals( Object object)
    {
    ParamData other =
      object instanceof ParamData
      ? (ParamData) object
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
      .append( getLocation())
      .append( getName())
      .appendSuper( super.toString())
      .toString();
    }

  private final String name_;
  private Location location_;
  private String style_;
  private boolean exploded_;
  }
