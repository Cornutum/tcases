//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.SystemTestDef;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;

/**
 * Provides access to system test definition resources.
 *
 * @version $Revision$, $Date$
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
      return read( class_.getResourceAsStream( resource));
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
      writer.close();
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
   * Returns the {@link SystemTestDef} defined by the given resource.
   */
  public SystemTestDef read( InputStream stream) throws Exception
    {
    SystemTestDef systemTestDef = null;
    
    try
      {
      SystemTestDocReader reader = new SystemTestDocReader( stream);
      systemTestDef = reader.getSystemTestDef();
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return systemTestDef;
    }

  private Class<?> class_;
  }
