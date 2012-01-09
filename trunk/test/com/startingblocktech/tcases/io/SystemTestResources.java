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
