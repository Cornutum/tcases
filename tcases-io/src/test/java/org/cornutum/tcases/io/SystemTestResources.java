//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ClassUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;

/**
 * Provides access to system test definition resources.
 *
 */
public class SystemTestResources
  {

  /**
   * Creates a new SystemTestResources object.
   */
  public SystemTestResources( Class<?> type)
    {
    class_ = type;
    }

  /**
   * Returns the {@link SystemTestDef} defined by the given resource.
   */
  public SystemTestDef read( String resource)
    {
    try
      {
      InputStream resourceStream = class_.getResourceAsStream( resource);
      if( resourceStream == null)
        {
        throw
          new RuntimeException
          ( "Can't find resource=" + class_.getName() + "." + resource);
        }
      return read( resourceStream);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    }

  /**
   * Returns the {@link SystemTestDef} defined by the given file.
   */
  public SystemTestDef read( File file)
    {
    try
      {
      return read( new FileInputStream( file));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read file=" + file, e);
      }
    }

  /**
   * Writes the {@link SystemTestDef} to the the given file.
   */
  public void write( SystemTestDef systemTest, File file)
    {
    SystemTestDocWriter writer = createWriter( file);
    try
      {
      writer.write( systemTest);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write " + systemTest + " to file=" + file, e);
      }
    finally
      {
      IOUtils.closeQuietly( writer);
      }
    }

  /**
   * Creates a {@link SystemTestDocWriter} for the given file.
   */
  private SystemTestDocWriter createWriter( File file)
    {
    try
      {
      return new SystemTestDocWriter( new FileWriter( file));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't open file=" + file, e);
      }
    }

  /**
   * Returns the {@link SystemTestDef} defined by the given stream.
   */
  public SystemTestDef read( InputStream stream) throws Exception
    {
    try( SystemTestDocReader reader = new SystemTestDocReader( stream))
      {
      return reader.getSystemTestDef();
      }
    }

  /**
   * Returns the {@link SystemTestDef} defined by the given JSON resource.
   */
  public SystemTestDef readJson( String resource)
    {
    SystemTestDef  systemTestDef  = null;
    InputStream     stream          = null;
    
    stream = class_.getResourceAsStream( resource);
    if( stream == null)
      {
      throw
        new RuntimeException
        ( "Can't find resource=" + ClassUtils.getPackageName( class_) + "." + resource);
      }

    try( SystemTestJsonReader reader = new SystemTestJsonReader( stream))
      {
      systemTestDef = reader.getSystemTestDef();
      }

    return systemTestDef;
    }

  /**
   * Returns the {@link SystemTestDef} defined by the given JSON file.
   */
  public SystemTestDef readJson( File file)
    {
    SystemTestDef  systemTestDef  = null;
    try( SystemTestJsonReader reader = new SystemTestJsonReader( new FileInputStream( file)))
      {
      systemTestDef = reader.getSystemTestDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read file=" + file, e);
      }

    return systemTestDef;
    }

  private Class<?> class_;
  }
