//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.io.IndentedWriter;

/**
 * POJO configuration for a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md">Moco server</a>.
 */
public class MocoServerConfigPojo extends MocoServerConfig
  {
  /**
   * Creates a new MocoServerConfigPojo instance.
   */
  public MocoServerConfigPojo()
    {
    this( null);
    }
  
  /**
   * Creates a new MocoServerConfigPojo instance.
   */
  public MocoServerConfigPojo( PojoWriter pojoWriter)
    {
    pojoWriter_ = pojoWriter;
    }

  /**
   * Returns the {@link PojoWriter} for this server configuration.
   */
  public PojoWriter getPojoWriter()
    {
    return pojoWriter_;
    }

  /**
   * Implements the Visitor pattern for this server configuration.
   */
  public void accept( ConfigVisitor visitor)
    {
    visitor.visit( this);
    }

  private final PojoWriter pojoWriter_;

  /**
   * Writes the Mojo API code to configure the given server.
   */
  public interface PojoWriter
    {
    /**
     * Writes the Mojo API code to configure the given server.
     */
    public void writePojo( String serverName, IndentedWriter targetWriter);
    }

  }
