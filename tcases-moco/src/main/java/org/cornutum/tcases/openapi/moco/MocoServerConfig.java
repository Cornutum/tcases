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
 * Base class for the configuration for a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md">Moco server</a>.
 */
public abstract class MocoServerConfig
  {
  /**
   * Creates a new MocoServerConfig instance.
   */
  protected MocoServerConfig()
    {
    setPort( null);
    setName( null);
    setEachTest( false);
    }

  /**
   * Changes the local port used by the Moco server to listen for requests.
   * If null, sets the default port.
   */
  public void setPort( Integer port)
    {
    port_ = Optional.ofNullable( port).orElse( 12306);
    }

  /**
   * Returns the local port used by the Moco server to listen for requests.
   */
  public Integer getPort()
    {
    return port_;
    }

  /**
   * Changes the variable name used to reference the Moco server.
   * If null, sets the default variable name
   */
  public void setName( String name)
    {
    name_ = Optional.ofNullable( name).orElse( "mocoServer");
    }

  /**
   * Returns the variable name used to reference the Moco server.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Changes if the Moco server is restarted for each @Test method (true) or only once for each test class.
   */
  public void setEachTest( boolean eachTest)
    {
    eachTest_ = eachTest;
    }

  /**
   * Returns if the Moco server is restarted for each @Test method (true) or only once for each test class.
   */
  public boolean isEachTest()
    {
    return eachTest_;
    }

  /**
   * Returns the JUnit TestRule type for this server configuration.
   */
  public String getRuleType()
    {
    return isEachTest()? "Rule" : "ClassRule";
    }

  /**
   * Implements the Visitor pattern for this server configuration.
   */
  public abstract void accept( ConfigVisitor visitor);

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .append( getPort())
      .append( getRuleType())
      .build();
    }

  /**
   * Creates a new Builder for a MocoServerConfigFile instance.
   */
  public static Builder file( File file)
    {
    return new Builder( new MocoServerConfigFile( file));
    }
    
  /**
   * Creates a new Builder for a MocoServerConfigFile instance.
   */
  public static Builder file( String file)
    {
    return new Builder( new MocoServerConfigFile( file));
    }
    
  /**
   * Creates a new Builder for a MocoServerConfigResource instance.
   */
  public static Builder resource( String resource)
    {
    return new Builder( new MocoServerConfigResource( resource));
    }
    
  /**
   * Creates a new Builder for a MocoServerConfigPojo instance.
   */
  public static Builder pojo()
    {
    return pojo( null);
    }
    
  /**
   * Creates a new Builder for a MocoServerConfigPojo instance.
   */
  public static Builder pojo( MocoServerConfigPojo.PojoWriter pojo)
    {
    return new Builder( new MocoServerConfigPojo( pojo));
    }
  private Integer port_;
  private String name_;
  private boolean eachTest_;

  /**
   * Builds a {@link MocoServerConfig} instance
   */
  public static class Builder
    {
    /**
     * Creates a new Builder instance.
     */
    private Builder( MocoServerConfig config)
      {
      config_ = config;
      }

    public Builder name( String name)
      {
      config_.setName( name);
      return this;
      }

    public Builder port( int port)
      {
      config_.setPort( port);
      return this;
      }

    public Builder forEachTest( boolean eachTest)
      {
      config_.setEachTest( eachTest);
      return this;
      }

    public Builder forEachTest()
      {
      return forEachTest( true);
      }

    public MocoServerConfig build()
      {
      return config_;
      }
    
    private MocoServerConfig config_;
    }
  
  }
