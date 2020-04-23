//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

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

  private Integer port_;
  private String name_;
  private boolean eachTest_;
  }
