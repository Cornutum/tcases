//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

/**
 * Describes an instance of a request header.
 */
public class HeaderDef extends ParamDef
  {
  /**
   * Creates a new HeaderDef instance.
   */
  public HeaderDef( String name)
    {
    super( name);
    setLocation( Location.HEADER);
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

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .append( getValue())
      .toString();
    }
  }
