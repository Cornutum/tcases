//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Builds a {@link HeaderData} instance.
 */
public class HeaderDataBuilder extends AbstractMessageDataBuilder<HeaderDataBuilder>
  {
  public static HeaderDataBuilder header( String name)
    {
    return new HeaderDataBuilder().name( name);
    }

  public HeaderDataBuilder name( String name)
    {
    name_ = name;
    return this;
    }

  public HeaderDataBuilder exploded()
    {
    return exploded( true);
    }

  public HeaderDataBuilder exploded( boolean exploded)
    {
    exploded_ = exploded;
    return this;
    }


  @Override
  public HeaderData build()
    {
    HeaderData header = new HeaderData( name_, super.build());
    header.setExploded( exploded_);

    return header;
    }
  
  private String name_;
  private boolean exploded_;
  }
