//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * An {@link ISystemInputSource} that reads from an JSON document.
 *
 */
public class SystemInputJsonReader implements ISystemInputSource, Closeable
  {  
  /**
   * Creates a new SystemInputJsonReader object.
   */
  public SystemInputJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemInputJsonReader object.
   */
  public SystemInputJsonReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link SystemInputDef} instance.
   */
  @Override
public SystemInputDef getSystemInputDef()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/system-input-schema.json"));
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
        throw new SystemInputException( "Invalid system input definition", e);
        }

      return SystemInputJson.asSystemInputDef( json);
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
