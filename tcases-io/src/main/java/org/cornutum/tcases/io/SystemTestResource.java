//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.InputStream;
import java.net.URL;

/**
 * An {@link ISystemTestSource} that returns the {@link SystemTestDef} at the given URL.
 */
public class SystemTestResource implements ISystemTestSource, Closeable
  {
  /**
   * Creates a new SystemTestResource instance.
   */
  public SystemTestResource( URL location)
    {
    location_ = location;
    }

  /**
   * Returns a {@link SystemTestDef} instance.
   */
  public SystemTestDef getSystemTestDef()
    {
    String resourceType = FilenameUtils.getExtension( location_.getPath());

    ISystemTestSource source;
    try
      {
      if( "json".equalsIgnoreCase( resourceType))
        {
        source = new SystemTestJsonReader( (stream_ = location_.openStream()));
        }
      else if( "xml".equalsIgnoreCase( resourceType))
        {
        source = new SystemTestDocReader( (stream_ = location_.openStream()));
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

    return source.getSystemTestDef();
    }

  /**
   * Returns the {@link SystemTestResource} at the given location.
   */
  public static SystemTestResource at( URL location)
    {
    return new SystemTestResource( location);
    }

  public void close()
    {
    IOUtils.closeQuietly( stream_);
    }
  
  private final URL location_;
  private InputStream stream_;
  }
