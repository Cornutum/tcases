//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;

import org.apache.commons.io.IOUtils;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.util.function.Function;

/**
 * Provides access to system input definition resources.
 *
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
   * Writes the {@link SystemInputDef} to the the given file.
   */
  public void write( SystemInputDef systemInput, File file)
    {
    SystemInputDocWriter writer = createWriter( file);
    try
      {
      writer.write( systemInput);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write " + systemInput + " to file=" + file, e);
      }
    finally
      {
      IOUtils.closeQuietly( writer, null);
      }
    }

  /**
   * Creates a {@link SystemInputDocWriter} for the given file.
   */
  private SystemInputDocWriter createWriter( File file)
    {
    try
      {
      return new SystemInputDocWriter( new FileWriter( file));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't open file=" + file, e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given resource.
   */
  public SystemInputDef read( String resource)
    {
    try
      {
      InputStream stream = class_.getResourceAsStream( resource);
      if( stream == null)
        {
        throw
          new RuntimeException
          ( "Can't find resource=" + class_.getName() + "." + resource);
        }

      return read( stream);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given file.
   */
  public SystemInputDef read( File file)
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
   * Returns the {@link SystemInputDef} defined by the given string.
   */
  public SystemInputDef readString( String string)
    {
    try
      {
      return read( new ByteArrayInputStream( string.getBytes( "UTF-8")));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read string", e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given stream.
   */
  public SystemInputDef read( InputStream stream) throws Exception
    {
    try( SystemInputDocReader reader = new SystemInputDocReader( stream))
      {
      return reader.getSystemInputDef();
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given JSON resource.
   */
  public SystemInputDef readJson( String resource)
    {
    try
      {
      InputStream stream  = class_.getResourceAsStream( resource);
      if( stream == null)
        {
        throw
          new RuntimeException
          ( "Can't find resource=" + class_.getName() + "." + resource);
        }

      return readJson( stream);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read resource=" + resource, e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given JSON file.
   */
  public SystemInputDef readJson( File file)
    {
    try
      {
      return readJson( new FileInputStream( file));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read file=" + file, e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given JSON string.
   */
  public SystemInputDef readJsonString( String string)
    {
    try
      {
      return readJson( new ByteArrayInputStream( string.getBytes( "UTF-8")));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read string", e);
      }
    }

  /**
   * Returns the {@link SystemInputDef} defined by the given JSON stream.
   */
  public SystemInputDef readJson( InputStream stream) throws Exception
    {
    try( SystemInputJsonReader reader = getJsonReaderProducer().apply( stream))
      {
      return reader.getSystemInputDef();
      }
    }

  /**
   * Writes the {@link SystemInputDef} to the the given file.
   */
  public void writeJson( SystemInputDef systemInputDef, File file)
    {
    try( SystemInputJsonWriter writer = new SystemInputJsonWriter( new FileOutputStream( file)))
      {
      writer.write( systemInputDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write file=" + file, e);
      }
    }

  /**
   * Changes the producer of a JSON reader.
   */
  protected void setJsonReaderProducer( Function<InputStream,SystemInputJsonReader> producer)
    {
    jsonReaderProducer_ = producer;
    }

  /**
   * Returns the producer of a JSON reader.
   */
  protected Function<InputStream,SystemInputJsonReader> getJsonReaderProducer()
    {
    return jsonReaderProducer_;
    }

  private Class<?> class_;
  private Function<InputStream,SystemInputJsonReader> jsonReaderProducer_ = SystemInputJsonReader::new;
  }
