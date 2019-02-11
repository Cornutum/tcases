//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;

/**
 * Provides access to system input definition resources.
 *
 */
public class SystemInputResources
  {

  /**
   * Creates a new SystemInputResources object.
   */
  public SystemInputResources( Class<?> type)
    {
    class_ = type;
    }

  /**
   * Writes the {@link SystemInputDef} to the the given file.
   */
  public void write( SystemInputDef systemInput, File file)
    {
    SystemInputDocWriter writer = createWriter( file);
    try
      {
      writer.write( systemInput);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write " + systemInput + " to file=" + file, e);
      }
    finally
      {
      IOUtils.closeQuietly( writer);
      }
    }

  /**
   * Creates a {@link SystemInputDocWriter} for the given file.
   */
  private SystemInputDocWriter createWriter( File file)
    {
    try
      {
      return new SystemInputDocWriter( new FileWriter( file));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't open file=" + file, e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given resource.
   */
  public SystemInputDef read( String resource)
    {
    SystemInputDef  systemInputDef  = null;
    InputStream     stream          = null;
    
    try
      {
      stream = class_.getResourceAsStream( resource);
      if( stream == null)
        {
        throw
          new RuntimeException
          ( "Can't find resource=" + class_.getName() + "." + resource);
        }

      SystemInputDocReader reader = new SystemInputDocReader( stream);
      systemInputDef = reader.getSystemInputDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return systemInputDef;
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given file.
   */
  public SystemInputDef read( File file)
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
   * Returns the {@link SystemInputDef} defined by the given stream.
   */
  public SystemInputDef read( InputStream stream) throws Exception
    {
    SystemInputDef systemInputDef = null;
    
    try
      {
      SystemInputDocReader reader = new SystemInputDocReader( stream);
      systemInputDef = reader.getSystemInputDef();
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return systemInputDef;
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given JSON resource.
   */
  public SystemInputDef readJson( String resource)
    {
    SystemInputDef  systemInputDef  = null;
    InputStream     stream          = null;
    
    stream = class_.getResourceAsStream( resource);
    if( stream == null)
      {
      throw
        new RuntimeException
        ( "Can't find resource=" + TestSystemInputJson.class.getName() + "." + resource);
      }

    try( SystemInputJsonReader reader = new SystemInputJsonReader( stream))
      {
      systemInputDef = reader.getSystemInputDef();
      }

    return systemInputDef;
    }

  private Class<?> class_;
  }
