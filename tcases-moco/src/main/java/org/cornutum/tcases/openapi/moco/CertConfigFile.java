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
 * File configuration for an <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/usage.md#https">HTTPS certificate</a>.
 */
public class CertConfigFile extends CertConfig
  {
  /**
   * Creates a new CertConfigFile instance.
   */
  public CertConfigFile( File file, String keyStorePassword, String certPassword)
    {
    this( Optional.ofNullable( file).map( File::getPath).orElse( null), keyStorePassword, certPassword);
    }
  
  /**
   * Creates a new CertConfigFile instance.
   */
  public CertConfigFile( String file, String keyStorePassword, String certPassword)
    {
    this( null, file, keyStorePassword, certPassword);
    }
  
  /**
   * Creates a new CertConfigFile instance.
   */
  public CertConfigFile( String name, File file, String keyStorePassword, String certPassword)
    {
    this( name, Optional.ofNullable( file).map( File::getPath).orElse( null), keyStorePassword, certPassword);
    }
  
  /**
   * Creates a new CertConfigFile instance.
   */
  public CertConfigFile( String name, String file, String keyStorePassword, String certPassword)
    {
    super( name, keyStorePassword, certPassword);
    path_ = Optional.ofNullable( file).orElseThrow( () -> new IllegalArgumentException( "Certificate config file path undefined"));
    }

  /**
   * Returns the file path for this server configuration.
   */
  public String getPath()
    {
    return path_;
    }

  /**
   * Implements the Visitor pattern for this certificate configuration.
   */
  @Override
public void accept( CertConfigVisitor visitor)
    {
    visitor.visit( this);
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getPath())
      .build();
    }

  private final String path_;
  }
