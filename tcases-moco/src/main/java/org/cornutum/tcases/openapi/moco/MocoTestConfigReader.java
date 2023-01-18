//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * Reads a {@link MocoTestConfig} object from a JSON document.
 *
 */
public class MocoTestConfigReader implements Closeable
  {
  /**
   * Creates a new MocoTestConfigReader object.
   */
  public MocoTestConfigReader()
    {
    this( null);
    }
  
  /**
   * Creates a new MocoTestConfigReader object.
   */
  public MocoTestConfigReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link MocoTestConfig} instance.
   */
  public MocoTestConfig getMocoTestConfig()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/moco-test-schema.json"));
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
        throw new MocoTestConfigException( "Invalid Moco test configuration", e);
        }

      return MocoTestConfigJson.asMocoTestConfig( json);
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
