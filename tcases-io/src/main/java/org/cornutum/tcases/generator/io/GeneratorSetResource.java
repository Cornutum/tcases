//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.IGeneratorSet;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.InputStream;
import java.net.URL;

/**
 * An {@link IGeneratorSetSource} that returns the {@link IGeneratorSet} at the given URL.
 */
public class GeneratorSetResource implements IGeneratorSetSource, Closeable
  {
  /**
   * Creates a new GeneratorSetResource instance.
   */
  public GeneratorSetResource( URL location)
    {
    location_ = location;
    }

  /**
   * Returns a {@link IGeneratorSet} instance.
   */
  @SuppressWarnings("resource")
  public IGeneratorSet getGeneratorSet()
    {
    IGeneratorSetSource source;
    try
      {
      String resourceType = FilenameUtils.getExtension( location_.getPath());

      if( "json".equalsIgnoreCase( resourceType))
        {
        source = new GeneratorSetJsonReader( (stream_ = location_.openStream()));
        }
      else if( "xml".equalsIgnoreCase( resourceType))
        {
        source = new GeneratorSetDocReader( (stream_ = location_.openStream()));
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

    return source.getGeneratorSet();
    }

  public void close()
    {
    IOUtils.closeQuietly( stream_);
    }

  /**
   * Returns the {@link GeneratorSetResource} at the given location.
   */
  public static GeneratorSetResource at( URL location)
    {
    return new GeneratorSetResource( location);
    }

  private final URL location_;
  private InputStream stream_;
  }
