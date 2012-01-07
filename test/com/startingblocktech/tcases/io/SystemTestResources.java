//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.SystemTestDef;

import org.apache.commons.io.IOUtils;

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
    SystemTestDef  systemTestDef  = null;
    InputStream     stream          = null;
    
    try
      {
      stream = class_.getResourceAsStream( resource);

      SystemTestDocReader reader = new SystemTestDocReader( stream);
      systemTestDef = reader.getSystemTestDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return systemTestDef;
    }

  private Class<?> class_;
  }
