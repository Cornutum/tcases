//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;
import static org.cornutum.tcases.io.Resource.Type.*;
import java.io.File;
import java.net.URL;

/**
 * An {@link ISystemTestSource} that returns the {@link SystemTestDef} at the given URL.
 */
public class SystemTestResource extends Resource implements ISystemTestSource
  {
  /**
   * Creates a new SystemTestResource instance.
   */
  public SystemTestResource( URL location)
    {
    super( location);
    }
  
  /**
   * Creates a new SystemTestResource instance.
   */
  public SystemTestResource( File location)
    {
    super( location);
    }

  /**
   * Returns a {@link SystemTestDef} instance.
   */
  @Override
  public SystemTestDef getSystemTestDef()
    {
    return
      getType() == XML?
      new SystemTestDocReader( openInput()).getSystemTestDef() :
      new SystemTestJsonReader( openInput()).getSystemTestDef();
    }

  /**
   * Writes the given system test definition.
   */
  public void write( SystemTestDef systemTest)
    {
    if( getType() == XML)
      {
      try( SystemTestDocWriter writer = new SystemTestDocWriter( openOutput()))
        {
        writer.write( systemTest);
        }
      }
    else
      {
      try( SystemTestJsonWriter writer = new SystemTestJsonWriter( openOutput()))
        {
        writer.write( systemTest);
        }
      }
    }

  /**
   * Returns the {@link SystemTestResource} at the given location.
   */
  public static SystemTestResource at( URL location)
    {
    return new SystemTestResource( location);
    }

  /**
   * Returns the {@link SystemTestResource} for the given file.
   */
  public static SystemTestResource of( File file)
    {
    return new SystemTestResource( file);
    }
  }
