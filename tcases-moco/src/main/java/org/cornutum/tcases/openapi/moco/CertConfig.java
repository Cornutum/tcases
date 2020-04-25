//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import java.util.Optional;

/**
 * Base class for the configuration for an <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/usage.md#https">HTTPS certificate</a>.
 */
public abstract class CertConfig
  {
  /**
   * Creates a new CertConfig instance.
   */
  protected CertConfig( String name, String keyStorePassword, String certPassword)
    {
    name_ =
      Optional.ofNullable( name)
      .orElse( "DEFAULT_CERTIFICATE");

    keyStorePassword_ =
      Optional.ofNullable( keyStorePassword)
      .orElseThrow( () -> new IllegalArgumentException( "The key store password must be defined"));

    certPassword_ =
      Optional.ofNullable( certPassword)
      .orElseThrow( () -> new IllegalArgumentException( "The certificat password must be defined"));
    }

  /**
   * Returns the variable name for the certificate
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Returns the key store password for the certificate
   */
  public String getKeyStorePassword()
    {
    return keyStorePassword_;
    }

  /**
   * Returns the certificate password for the certificate
   */
  public String getCertPassword()
    {
    return certPassword_;
    }

  /**
   * Implements the Visitor pattern for this certificate configuration.
   */
  public abstract void accept( CertConfigVisitor visitor);

  private final String name_;
  private final String keyStorePassword_;
  private final String certPassword_;  
  }
