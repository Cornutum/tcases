//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.io.FileReader;

/**
 * Base class for response tests
 */
public abstract class ResponseTest
  {
  /**
   * Reads response definitions from a resource file.
   */
  protected ResponsesDef readResponses( String resource)
    {
    try( FileReader reader = new FileReader( getClass().getResource( resource + ".json").getFile()))
      {
      return ResponsesDef.read( reader);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read ResponsesDef", e);
      }
    }
  }
