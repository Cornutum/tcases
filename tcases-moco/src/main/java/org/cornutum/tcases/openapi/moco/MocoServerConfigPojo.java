//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import java.lang.reflect.InvocationTargetException;
import java.util.Optional;

import org.cornutum.tcases.io.IndentedWriter;

/**
 * POJO configuration for a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md">Moco server</a>.
 */
public class MocoServerConfigPojo extends MocoServerConfig
  {  
  /**
   * Creates a new MocoServerConfigPojo instance.
   */
  public MocoServerConfigPojo( PojoWriterFactory factory)
    {
    setFactory( factory);
    }
  
  /**
   * Creates a new MocoServerConfigPojo instance.
   */
  public MocoServerConfigPojo( Class<PojoWriterFactory> factoryClass) throws InstantiationException, IllegalAccessException, InvocationTargetException, IllegalArgumentException, NoSuchMethodException, SecurityException
    {
    this( factoryClass.getDeclaredConstructor().newInstance());
    }
  
  /**
   * Creates a new MocoServerConfigPojo instance.
   */
  @SuppressWarnings("unchecked")
  public MocoServerConfigPojo( String factoryClassName) throws InstantiationException, IllegalAccessException, ClassNotFoundException, InvocationTargetException, IllegalArgumentException, NoSuchMethodException, SecurityException
    {
    this( (Class<PojoWriterFactory>) Class.forName( factoryClassName));
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
    if( pojoWriter_ == null)
      {
      pojoWriter_ =
        Optional.ofNullable( getFactory())
        .map( PojoWriterFactory::createPojoWriter)
        .orElse( null);
      }
    
    return pojoWriter_;
    }

  /**
   * Changes the {@link PojoWriterFactory} for this server configuration.
   */
  public void setFactory( PojoWriterFactory factory)
    {
    factory_ = factory;
    }

  /**
   * Returns the {@link PojoWriterFactory} for this server configuration.
   */
  public PojoWriterFactory getFactory()
    {
    return factory_;
    }

  /**
   * Implements the Visitor pattern for this server configuration.
   */
  @Override
public void accept( ConfigVisitor visitor)
    {
    visitor.visit( this);
    }

  private PojoWriter pojoWriter_;
  private PojoWriterFactory factory_;

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
  
  /**
   * Writes the Mojo API code to configure the given server.
   */
  public interface PojoWriterFactory
    {
    /**
     * Writes the Mojo API code to configure the given server.
     */
    public PojoWriter createPojoWriter();
    }

  }
