//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.InputStream;
import java.net.URL;

/**
 * An {@link ISystemInputSource} that returns the {@link SystemInputDef} at the given URL.
 */
public class SystemInputResource implements ISystemInputSource, Closeable
  {
  /**
   * Creates a new SystemInputResource instance.
   */
  public SystemInputResource( URL location)
    {
    location_ = location;
    }

  /**
   * Returns a {@link SystemInputDef} instance.
   */
  @SuppressWarnings("resource")
  public SystemInputDef getSystemInputDef()
    {
    ISystemInputSource source;
    try
      {
      String resourceType = FilenameUtils.getExtension( location_.getPath());

      if( "json".equalsIgnoreCase( resourceType))
        {
        source = new SystemInputJsonReader( (stream_ = location_.openStream()));
        }
      else if( "xml".equalsIgnoreCase( resourceType))
        {
        source = new SystemInputDocReader( (stream_ = location_.openStream()));
        }
      else
        {
        throw new IllegalStateException( String.format( "Unknown resource type='%s'", resourceType));
        }
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't read resource at %s", location_));
      }

    return source.getSystemInputDef();
    }

  public void close()
    {
    IOUtils.closeQuietly( stream_);
    }

  /**
   * Returns the {@link SystemInputResource} at the given location.
   */
  public static SystemInputResource at( URL location)
    {
    return new SystemInputResource( location);
    }
  
  private final URL location_;
  private InputStream stream_;
  }
