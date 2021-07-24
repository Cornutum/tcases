//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;
import static org.cornutum.tcases.io.Resource.Type.JSON;
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
      getType() == JSON?
      new SystemTestJsonReader( open()).getSystemTestDef() :

      new SystemTestDocReader( open()).getSystemTestDef();
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
