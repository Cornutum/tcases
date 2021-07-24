//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.ParamDef.Location;

/**
 * Builds a {@link ParamData} instance.
 */
public class ParamDataBuilder extends AbstractMessageDataBuilder<ParamDataBuilder>
  {
  public static ParamDataBuilder param( String name)
    {
    return new ParamDataBuilder().name( name);
    }

  public ParamDataBuilder name( String name)
    {
    name_ = name;
    return this;
    }

  public ParamDataBuilder location( Location location)
    {
    location_ = location;
    return this;
    }

  public ParamDataBuilder style( String style)
    {
    style_ = style;
    return this;
    }

  public ParamDataBuilder exploded()
    {
    return exploded( true);
    }

  public ParamDataBuilder exploded( boolean exploded)
    {
    exploded_ = exploded;
    return this;
    }


  @Override
  public ParamData build()
    {
    ParamData param = new ParamData( name_, super.build());
    param.setLocation( location_);
    param.setStyle( style_);
    param.setExploded( exploded_);

    return param;
    }
  
  private String name_;
  private Location location_;
  private String style_;
  private boolean exploded_;
  }
