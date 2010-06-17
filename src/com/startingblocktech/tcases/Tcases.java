//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.io.SystemInputDocReader;

import org.apache.commons.io.IOUtils;

import java.io.FileInputStream;

/**
 * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}.
 *
 * @version $Revision$, $Date$
 */
public class Tcases
  {
  /**
   * Creates a new Tcases object.
   */
  public Tcases()
    {
    }

  /**
   * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}.
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      String file = args[0];
      
      FileInputStream stream = null;
      try
        {
        stream = new FileInputStream( file);
        SystemInputDocReader reader = new SystemInputDocReader( stream);
        SystemInputDef inputDef = reader.getSystemInputDef();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read file=" + file, e);
        }
      finally
        {
        IOUtils.closeQuietly( stream);
        }
      }
    catch( Exception e)
      {
      exitCode = 1;
      e.printStackTrace();
      }
    finally
      {
      System.exit( exitCode);
      }
    }
  }

