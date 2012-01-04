//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.SystemInputDef;

import org.apache.commons.io.IOUtils;

import java.io.InputStream;

/**
 * Provides access to system input definition resources.
 *
 * @version $Revision$, $Date$
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
   * Returns the {@link SystemInputDef} defined by the given resource.
   */
  public SystemInputDef read( String resource)
    {
    SystemInputDef  systemInputDef  = null;
    InputStream     stream          = null;
    
    try
      {
      stream = class_.getResourceAsStream( resource);

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

  private Class<?> class_;
  }
