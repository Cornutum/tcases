//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.util.ToString;

import java.io.File;
import java.util.Optional;

/**
 * File configuration for a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md">Moco server</a>.
 */
public class MocoServerConfigFile extends MocoServerConfig
  {
  /**
   * Creates a new MocoServerConfigFile instance.
   */
  public MocoServerConfigFile( File file)
    {
    this( Optional.ofNullable( file).map( File::getPath).orElse( null));
    }
  
  /**
   * Creates a new MocoServerConfigFile instance.
   */
  public MocoServerConfigFile( String file)
    {
    path_ = Optional.ofNullable( file).orElseThrow( () -> new IllegalArgumentException( "Server config file path undefined"));
    }

  /**
   * Returns the file path for this server configuration.
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

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .appendSuper( super.toString())
      .append( "path", getPath())
      .build();
    }

  private final String path_;
  }
