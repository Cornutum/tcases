//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.IGeneratorSet;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * An {@link IGeneratorSetSource} that reads from an JSON document.
 *
 */
public class GeneratorSetJsonReader implements IGeneratorSetSource, Closeable
  {  
  /**
   * Creates a new GeneratorSetJsonReader object.
   */
  public GeneratorSetJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new GeneratorSetJsonReader object.
   */
  public GeneratorSetJsonReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link IGeneratorSet} instance.
   */
  @Override
  public IGeneratorSet getGeneratorSet()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/generators-schema.json"));
    ProblemHandler handler = ProblemHandler.throwing();
    try( JsonReader reader = service.createReader( stream_, schema, handler))
      {
      JsonObject json;
      try
        {
        json = reader.readObject();
        }
      catch( Exception e)
        {
        throw new GeneratorSetException( "Invalid generator set definition", e);
        }

      return GeneratorSetJson.asGeneratorSet( json);
      }
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ =
      stream==null
      ? System.in
      : stream;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return stream_;
    }

  @Override
  public void close()
    {
    IOUtils.closeQuietly( getInputStream(), null);
    }

  private InputStream stream_;
  }
