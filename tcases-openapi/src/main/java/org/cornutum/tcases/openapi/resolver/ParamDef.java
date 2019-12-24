//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

/**
 * Describes an instance of a request parameter.
 */
public class ParamDef
  {
  public enum Location{ QUERY, PATH, HEADER, COOKIE};
  
  /**
   * Creates a new ParamDef instance.
   */
  public ParamDef( String name)
    {
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
   * Changes the value definition for this parameter.
   */
  public void setValue( ValueDef<?> value) 
    {
    value_ = value;
    }

  /**
   * Returns the value definition for this parameter.
   */
  public ValueDef<?> getValue() 
    {
    return value_;
    }

  /**
   * Changes the location of the parameter value.
   */
  public void setLocation( String location)
    {
    try
      {
      setLocation(
        location == null
        ? null
        : Location.valueOf( location.toUpperCase()));
      }
    catch( Exception e)
      {
      throw new RequestCaseException( "Can't set location", e);
      }
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

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getLocation())
      .append( getName())
      .append( getValue())
      .toString();
    }

  private final String name_;
  private ValueDef<?> value_;
  private Location location_;
  private String style_;
  private boolean exploded_;
  }
