//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * An {@link ISystemTestSource} that reads from an JSON document.
 *
 */
public class SystemTestJsonReader implements ISystemTestSource, Closeable
  {  
  /**
   * Creates a new SystemTestJsonReader object.
   */
  public SystemTestJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemTestJsonReader object.
   */
  public SystemTestJsonReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link SystemTestDef} instance.
   */
  @Override
public SystemTestDef getSystemTestDef()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/system-test-schema.json"));
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
        throw new SystemTestException( "Invalid system test definition", e);
        }

      return SystemTestJson.asSystemTestDef( json);
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
