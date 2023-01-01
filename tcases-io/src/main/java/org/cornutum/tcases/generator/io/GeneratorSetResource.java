//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.IGeneratorSet;
import org.cornutum.tcases.io.Resource;
import static org.cornutum.tcases.io.Resource.Type.*;
import java.io.File;
import java.net.URL;

/**
 * An {@link IGeneratorSetSource} that returns the {@link IGeneratorSet} at the given URL.
 */
public class GeneratorSetResource extends Resource implements IGeneratorSetSource
  {
  /**
   * Creates a new GeneratorSetResource instance.
   */
  public GeneratorSetResource( URL location)
    {
    super( location);
    }
  
  /**
   * Creates a new GeneratorSetResource instance.
   */
  public GeneratorSetResource( File location)
    {
    super( location);
    }

  /**
   * Returns a {@link IGeneratorSet} instance.
   */
  @Override
  public IGeneratorSet getGeneratorSet()
    {
    return
      getType() == XML?
      new GeneratorSetDocReader( openInput()).getGeneratorSet() :
      new GeneratorSetJsonReader( openInput()).getGeneratorSet() ;
    }

  /**
   * Writes the given generator set.
   */
  @SuppressWarnings("resource")
  public void write( IGeneratorSet generatorSet)
    {
    if( getType() == XML)
      {
      try( GeneratorSetDocWriter writer = new GeneratorSetDocWriter( openOutput()))
        {
        writer.write( generatorSet);
        }
      }
    else
      try( GeneratorSetJsonWriter writer = new GeneratorSetJsonWriter( openOutput()))
        {
        writer.write( generatorSet);
        }
    }

  /**
   * Returns the {@link GeneratorSetResource} at the given location.
   */
  public static GeneratorSetResource at( URL location)
    {
    return new GeneratorSetResource( location);
    }

  /**
   * Returns the {@link GeneratorSetResource} for the given file.
   */
  public static GeneratorSetResource of( File file)
    {
    return new GeneratorSetResource( file);
    }
  }
