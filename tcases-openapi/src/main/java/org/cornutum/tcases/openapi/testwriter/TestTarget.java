//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.File;
import java.io.OutputStream;

/**
 * Defines the target for output from a {@link TestWriter}.
 */
public class TestTarget
  {
  /**
   * Creates a new TestTarget instance.
   */
  public TestTarget()
    {
    }

  /**
   * Changes the output stream designated for this target.
   */
  public void setOutput( OutputStream output)
    {
    output_ = output;
    }

  /**
   * Returns the output stream designated for this target.
   */
  public OutputStream getOutput()
    {
    return output_;
    }

  /**
   * Changes the test name for this target.
   */
  public void setName( String name)
    {
    name_ = name;
    }

  /**
   * Returns the test name for this target.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Changes the output file for this target.
   */
  public void setFile( File file)
    {
    file_ = file;
    }

  /**
   * Changes the output file for this target.
   */
  public void setFile( String file)
    {
    setFile( new File( file));
    }

  /**
   * Returns the output file for this target.
   */
  public File getFile()
    {
    return file_;
    }

  /**
   * Changes the output directory for this target.
   */
  public void setDir( File dir)
    {
    dir_ = dir;
    }

  /**
   * Returns the output directory for this target.
   */
  public File getDir()
    {
    return dir_;
    }

  /**
   * Changes the test timeout (milliseconds) for this target.
   */
  public void setTimeout( Long millis)
    {
    timeout_ = millis;
    }

  /**
   * Returns the test timeout (milliseconds) for this target.
   */
  public Long getTimeout()
    {
    return timeout_;
    }

  @Override
  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    if( getName() != null)
      {
      builder.append( "name", getName());
      }
    
    if( getFile() != null && getDir() != null)
      {
      builder.append( "file", getFile().getName()).append( "dir", getDir());
      }
    else if( getFile() != null)
      {
      builder.append( "file", getFile());
      }
    else if( getDir() != null)
      {
      builder.append( "dir", getDir());
      }
    else if( getOutput() != null)
      {
      builder.append( getOutput().getClass().getSimpleName());
      }
    else
      {
      builder.append( "STDOUT");
      }

    if( getTimeout() != null)
      {
      builder.append( "timeout", getTimeout());
      }
    
    return builder.toString();    
    }

  /**
   * Returns a new {@link Builder}.
   */
  public static Builder basic()
    {
    return new Builder();
    }

  private OutputStream output_;
  private String name_;
  private File file_;
  private File dir_;
  private Long timeout_;

  /**
   * Builds a {@link TestTarget} instance.
   */
  public static class Builder extends BaseBuilder<Builder>
    {
    /**
     * Creates a new {@link Builder}
     */
    private Builder()
      {
      target_ = new TestTarget();
      }

    /**
     * Returns the {@link TestTarget} instance for this builder.
     */
    @Override
    protected TestTarget getTestTarget()
      {
      return target_;
      }
    
    /**
     * Returns the {@link TestTarget} instance for this builder.
     */
    public TestTarget build()
      {
      return target_;
      }

    private TestTarget target_;
    }

  /**
   * Base class for {@link TestTarget} builders.
   */
  @SuppressWarnings("unchecked")
  public static abstract class BaseBuilder<T extends BaseBuilder<T>>
    {
    /**
     * Returns the {@link TestTarget} instance for this builder.
     */
    protected abstract TestTarget getTestTarget();

    public T named( String name)
      {
      getTestTarget().setName( name);
      return (T) this;
      }

    public T toStream( OutputStream output)
      {
      getTestTarget().setOutput( output);
      return (T) this;
      }

    public T toFile( String file)
      {
      getTestTarget().setFile( file);
      return (T) this;
      }

    public T toFile( File file)
      {
      getTestTarget().setFile( file);
      return (T) this;
      }

    public T inDir( File dir)
      {
      getTestTarget().setDir( dir);
      return (T) this;
      }

    public T timeout( Long timeout)
      {
      getTestTarget().setTimeout( timeout);
      return (T) this;
      }
    }

  }
