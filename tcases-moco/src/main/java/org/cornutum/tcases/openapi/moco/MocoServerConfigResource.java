//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import java.util.Optional;

/**
 * Resource configuration for a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md">Moco server</a>.
 */
public class MocoServerConfigResource extends MocoServerConfig
  {
  /**
   * Creates a new MocoServerConfigResource instance.
   */
  public MocoServerConfigResource( String path)
    {
    path_ = Optional.ofNullable( path).orElseThrow( () -> new IllegalArgumentException( "Server config resource path undefined"));
    }

  /**
   * Returns the resource path for this server configuration.
   */
  public String getPath()
    {
    return path_;
    }

  /**
   * Implements the Visitor pattern for this server configuration.
   */
  public void accept( ConfigVisitor visitor)
    {
    visitor.visit( this);
    }

  private final String path_;
  }
