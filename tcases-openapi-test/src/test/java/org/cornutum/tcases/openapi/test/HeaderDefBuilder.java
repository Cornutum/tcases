//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

/**
 * Builds a {@link HeaderDef} instance.
 */
public class HeaderDefBuilder
  {
  public static HeaderDefBuilder header( String name)
    {
    return new HeaderDefBuilder().name( name);
    }

  public HeaderDefBuilder name( String name)
    {
    name_ = name;
    return this;
    }

  public HeaderDefBuilder required()
    {
    return required( true);
    }

  public HeaderDefBuilder required( boolean required)
    {
    required_ = required;
    return this;
    }

  /**
   * TBD
   */
  public HeaderDefBuilder contentDef( ContentDef contentDef)
    {
    contentDef_ = contentDef;
    return this;
    }

  public HeaderDef build()
    {
    return new HeaderDef( name_, required_, contentDef_);
    }
  
  private String name_;
  private boolean required_;
  private ContentDef contentDef_;
  }
