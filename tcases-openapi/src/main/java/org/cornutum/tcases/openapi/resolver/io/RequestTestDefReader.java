//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver.io;

import org.cornutum.tcases.openapi.resolver.RequestCaseException;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import javax.json.JsonArray;
import javax.json.JsonReader;

/**
 * Reads a {@link RequestTestDef} object from a JSON document.
 *
 */
public class RequestTestDefReader implements Closeable
  {  
  /**
   * Creates a new RequestTestDefReader object.
   */
  public RequestTestDefReader()
    {
    this( null);
    }
  
  /**
   * Creates a new RequestTestDefReader object.
   */
  public RequestTestDefReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link RequestTestDef} instance.
   */
  public RequestTestDef getRequestTestDef()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/request-cases-schema.json"));
    ProblemHandler handler = ProblemHandler.throwing();
    try( JsonReader reader = service.createReader( stream_, schema, handler))
      {
      JsonArray json;
      try
        {
        json = reader.readArray();
        }
      catch( Exception e)
        {
        throw new RequestCaseException( "Invalid request case definition", e);
        }

      return new RequestTestDef( RequestCaseJson.asRequestCases( json));
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
