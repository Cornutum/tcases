//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.util.ToString;

import java.util.Optional;

/**
 * Resource configuration for an <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/usage.md#https">HTTPS certificate</a>.
 */
public class CertConfigResource extends CertConfig
  { 
  /**
   * Creates a new CertConfigResource instance.
   */
  public CertConfigResource( String path, String keyStorePassword, String certPassword)
    {
    this( null, path, keyStorePassword, certPassword);
    }
   
  /**
   * Creates a new CertConfigResource instance.
   */
  public CertConfigResource( String name, String path, String keyStorePassword, String certPassword)
    {
    super( name, keyStorePassword, certPassword);
    path_ = Optional.ofNullable( path).orElseThrow( () -> new IllegalArgumentException( "Certificate config resource path undefined"));
    }

  /**
   * Returns the resource path for this server configuration.
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
