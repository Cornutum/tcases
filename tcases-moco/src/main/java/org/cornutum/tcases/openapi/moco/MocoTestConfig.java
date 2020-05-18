//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Optional;

/**
 * Defines how tests created by a MocoServerTestWriter will access a Moco server.
 */
public class MocoTestConfig
  {
  public enum ServerType { HttpServer, HttpsServer, RestServer, SocketServer };
  
  /**
   * Creates a new MocoTestConfig instance.
   */
  public MocoTestConfig()
    {
    this( (ServerType) null);
    }
  
  /**
   * Creates a new MocoTestConfig instance.
   */
  public MocoTestConfig( String serverType)
    {
    setServerType( serverType);
    }
  
  /**
   * Creates a new MocoTestConfig instance.
   */
  public MocoTestConfig( ServerType serverType)
    {
    setServerType( serverType);
    }

  /**
   * Changes the Moco server type.
   */
  public void setServerType( ServerType serverType)
    {
    serverType_ = serverType;
    }

  /**
   * Changes the Moco server type.
   */
  public void setServerType( String serverType)
    {
    setServerType(
      Optional.ofNullable( serverType)
      .map( ServerType::valueOf)
      .orElse( null));
    }

  /**
   * Returns the Moco server type.
   */
  public ServerType getServerType()
    {
    return serverType_;
    }

  /**
   * Changes the Moco server configuration.
   */
  public void setServerConfig( MocoServerConfig serverConfig)
    {
    serverConfig_ = serverConfig;
    }

  /**
   * Returns the Moco server configuration.
   */
  public MocoServerConfig getServerConfig()
    {
    return serverConfig_;
    }

  /**
   * Changes the Moco certificate configuration.
   */
  public void setCertConfig( CertConfig certConfig)
    {
    certConfig_ = certConfig;
    }

  /**
   * Returns the Moco certificate configuration.
   */
  public CertConfig getCertConfig()
    {
    return certConfig_;
    }

  public String toString()
    {
    ToStringBuilder builder = 
      ToString.getBuilder( this)
      .append( getServerType())
      .append( getServerConfig());

    Optional.ofNullable( getCertConfig()).ifPresent( cert -> builder.append( cert));

    return builder.build();
    }

  /**
   * Returns a {@link MocoServerTestWriter} defined by this test configuration.
   */
  public MocoServerTestWriter createTestWriter( TestCaseWriter testCaseWriter)
    {
    switch( getServerType())
      {
      case HttpServer:
        {
        return new HttpServerTestWriter( getServerConfig(), testCaseWriter);
        }

      case HttpsServer:
        {
        return new HttpsServerTestWriter( getServerConfig(), getCertConfig(), testCaseWriter);
        }

      case RestServer:
        {
        return new RestServerTestWriter( getServerConfig(), testCaseWriter);
        }

      case SocketServer:
        {
        return new SocketServerTestWriter( getServerConfig(), testCaseWriter);
        }

      default:
        {
        throw new MocoTestConfigException( String.format( "Can't create a MocoServerTestWriter for server type=%s", getServerType()));
        }
      }
    }

  /**
   * Returns a MocoTestConfig builder.
   */
  public static Builder builder( String serverType)
    {
    return new Builder( serverType);
    }

  /**
   * Returns a MocoTestConfig builder.
   */
  public static Builder builder( ServerType serverType)
    {
    return new Builder( serverType);
    }

  private ServerType serverType_;
  private MocoServerConfig serverConfig_;
  private CertConfig certConfig_;

  /**
   * Builds a {@link MocoTestConfig} instance
   */
  public static class Builder
    {
    /**
     * Creates a new Builder instance.
     */
    private Builder( String serverType)
      {
      testConfig_ = new MocoTestConfig( serverType);
      }
    
    /**
     * Creates a new Builder instance.
     */
    private Builder( ServerType serverType)
      {
      testConfig_ = new MocoTestConfig( serverType);
      }

    public Builder serverConfig( MocoServerConfig serverConfig)
      {
      testConfig_.setServerConfig( serverConfig);
      return this;
      }

    public Builder certConfig( CertConfig certConfig)
      {
      testConfig_.setCertConfig( certConfig);
      return this;
      }

    public MocoTestConfig build()
      {
      return testConfig_;
      }

    private MocoTestConfig testConfig_;
    }
  
  }
