//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;
import static org.cornutum.tcases.io.Resource.Type.*;
import java.io.File;
import java.net.URL;

/**
 * An {@link ISystemInputSource} that returns the {@link SystemInputDef} at the given URL.
 */
public class SystemInputResource extends Resource implements ISystemInputSource
  {
  /**
   * Creates a new SystemInputResource instance.
   */
  public SystemInputResource( URL location)
    {
    super( location);
    }
  
  /**
   * Creates a new SystemInputResource instance.
   */
  public SystemInputResource( File location)
    {
    super( location);
    }

  /**
   * Returns a {@link SystemInputDef} instance.
   */
  @Override
  public SystemInputDef getSystemInputDef()
    {
    return
      getType() == XML?
      new SystemInputDocReader( openInput()).getSystemInputDef() :
      new SystemInputJsonReader( openInput()).getSystemInputDef() ;
    }

  /**
   * Writes the given system input definition.
   */
  public void write( SystemInputDef systemInput)
    {
    if( getType() == XML)
      {
      try( SystemInputDocWriter writer = new SystemInputDocWriter( openOutput()))
        {
        writer.write( systemInput);
        }
      }
    else
      {
      try( SystemInputJsonWriter writer = new SystemInputJsonWriter( openOutput()))
        {
        writer.write( systemInput);
        }
      }
    }

  /**
   * Returns the {@link SystemInputResource} at the given location.
   */
  public static SystemInputResource at( URL location)
    {
    return new SystemInputResource( location);
    }

  /**
   * Returns the {@link SystemInputResource} for the given file.
   */
  public static SystemInputResource of( File file)
    {
    return new SystemInputResource( file);
    }
  }
