//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.IGeneratorSet;
import org.cornutum.tcases.io.Resource;
import static org.cornutum.tcases.io.Resource.Type.JSON;
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
  public IGeneratorSet getGeneratorSet()
    {
    return
      getType() == JSON?
      new GeneratorSetJsonReader( open()).getGeneratorSet() :

      new GeneratorSetDocReader( open()).getGeneratorSet();
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
