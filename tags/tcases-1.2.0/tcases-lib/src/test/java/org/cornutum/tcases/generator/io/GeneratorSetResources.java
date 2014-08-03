//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.IGeneratorSet;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;

/**
 * Provides access to generator definition resources.
 *
 * @version $Revision$, $Date$
 */
public class GeneratorSetResources
  {

  /**
   * Creates a new GeneratorSetResources object.
   */
  public GeneratorSetResources( Class<?> type)
    {
    class_ = type;
    }

  /**
   * Returns the {@link IGeneratorSet} defined by the given resource.
   */
  public IGeneratorSet read( String resource)
    {
    try
      {
      InputStream resourceStream = class_.getResourceAsStream( resource);
      if( resourceStream == null)
        {
        throw
          new RuntimeException
          ( "Can't find resource=" + class_.getName() + "." + resource);
        }
      return read( resourceStream);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    }

  /**
   * Returns the {@link IGeneratorSet} defined by the given file.
   */
  public IGeneratorSet read( File file)
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
   * Returns the {@link IGeneratorSet} defined by the given stream.
   */
  public IGeneratorSet read( InputStream stream) throws Exception
    {
    IGeneratorSet generatorSet = null;
    
    try
      {
      GeneratorSetDocReader reader = new GeneratorSetDocReader( stream);
      generatorSet = reader.getGeneratorSet();
      }
    finally
      {
      IOUtils.closeQuietly( stream);
      }

    return generatorSet;
    }

  /**
   * Writes the {@link IGeneratorSet} to the the given file.
   */
  public void write( IGeneratorSet generatorSet, File file)
    {
    GeneratorSetDocWriter writer = createWriter( file);
    try
      {
      writer.write( generatorSet);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write " + generatorSet + " to file=" + file, e);
      }
    finally
      {
      writer.close();
      }
    }

  /**
   * Creates a {@link GeneratorSetDocWriter} for the given file.
   */
  private GeneratorSetDocWriter createWriter( File file)
    {
    try
      {
      return new GeneratorSetDocWriter( new FileWriter( file));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't open file=" + file, e);
      }
    }

  private Class<?> class_;
  }
