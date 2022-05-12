//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.Optional;

/**
 * Describes the definition of a response header.
 */
public class HeaderDef
  {
  /**
   * Creates a new HeaderDef instance.
   */
  public HeaderDef( String name, Boolean required, ContentDef contentDef)
    {
    name_ = name;
    required_ = Optional.ofNullable( required).orElse( false);
    contentDef_ = contentDef;
    }

  /**
   * Returns the name of this header.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Returns if the header value is serialized in exploded form.
   */
  public boolean isExploded()
    {
    return getContentDef().isExploded();
    }

  /**
   * Returns if the header is required.
   */
  public boolean isRequired()
    {
    return required_;
    }

  /**
   * Returns the content definition for this header.
   */
  public ContentDef getContentDef()
    {
    return contentDef_;
    }

  @Override
  public String toString()
    {
    return
      ToString.builder( getClass())
      .add( getName())
      .addIf( "required=", Optional.of( isRequired()).filter( e -> e))
      .add( "content=", getContentDef())
      .toString();
    }

  private final String name_;
  private final boolean required_;
  private final ContentDef contentDef_;
  }
