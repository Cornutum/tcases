//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.Closeable;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.Optional;

/**
 * Base class for test project resources.
 */
public abstract class Resource implements Closeable
  {
  /**
   * Identifies the content type of a resource.
   */
  public enum Type
    {
      JSON, XML;

      /**
       * Returns the content type specified by the given string.
       */
      public static Type of( String string)
        {
        try
          {
          return valueOf( Type.class, StringUtils.trimToEmpty( string).toUpperCase());
          }
        catch( Exception ignore)
          {
          return null;
          }
        }

      /**
       * Returns the content type specified by the given URL.
       */
      public static Type of( URL location)
        {
        return
          location == null
          ? null
          : of( FilenameUtils.getExtension( location.getPath()));
        }

      /**
       * Returns the content type specified by the given file.
       */
      public static Type of( File location)
        {
        return
          location == null
          ? null
          : of( FilenameUtils.getExtension( location.getName()));
        }
    }

  /**
   * Creates a new Resource instance.
   */
  protected Resource( URL location)
    {
    location_ = location;
    setType( Type.of( location));
    }
  
  /**
   * Creates a new Resource instance.
   */
  protected Resource( File location)
    {
    this( urlFor( location));
    }

  /**
   * Returns the location of this resource.
   */
  protected URL getLocation()
    {
    return location_;
    }

  /**
   * Changes the content type of this resource.
   */
  public void setType( Type type)
    {
    type_ = type;
    }

  /**
   * Returns the content type of this resource.
   */
  public Type getType()
    {
    return type_;
    }

  /**
   * Opens a stream to read the contents of this resource.
   */
  protected InputStream openInput()
    {
    try
      {
      if( getType() == null)
        {
        throw new IllegalStateException( String.format( "Unknown type for %s", this));
        }

      URL location = getLocation();
    
      return
        location == null
        ? System.in
        : (inputStream_ = location.openStream());
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't read %s", this), e);
      }
    }

  /**
   * Opens a stream to write the contents of this resource.
   */
  protected OutputStream openOutput()
    {
    try
      {
      if( getType() == null)
        {
        throw new IllegalStateException( String.format( "Unknown type for %s", this));
        }

      URL location = getLocation();
    
      OutputStream outputStream =
        location == null?
        System.out :

        "file".equals( location.getProtocol())?
        (outputStream_ = new FileOutputStream( location.getPath())) :

        null;

      return
        Optional.ofNullable( outputStream)
        .orElseThrow( () -> new IllegalStateException( String.format( "Can't write to URL=%s", location)));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't write %s", this), e);
      }
    }

  @Override
  public void close()
    {
    IOUtils.closeQuietly( inputStream_);
    IOUtils.closeQuietly( outputStream_);
    }

  /**
   * Returns the URL for the given file.
   */
  public static URL urlFor( File file)
    {
    try
      {
      return
        file == null
        ? null
        : file.toURI().toURL();
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( "Can't get URL for file=" + file, e);
      }
    }

  /**
   * Returns the given resource, assigning the default type if no type yet defined.
   */
  public static <T extends Resource>  T withDefaultType( T resource, Type defaultType)
    {
    if( resource.getType() == null)
      {
      resource.setType( defaultType);
      }

    return resource;
    }

  /**
   * Returns the given file name, assigning the default type if no type defined.
   */
  public static File  withDefaultType( File file, Type defaultType)
    {
    return
      file != null && FilenameUtils.getExtension( file.getName()).isEmpty()
      ? new File( String.format( "%s.%s", file.getPath(), String.valueOf( defaultType).toLowerCase()))
      : file;
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getLocation())
      .toString();
    }

  private final URL location_;
  private Type type_;
  private InputStream inputStream_;
  private OutputStream outputStream_;
  }
