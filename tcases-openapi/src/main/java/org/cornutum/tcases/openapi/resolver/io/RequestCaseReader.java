//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver.io;

import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestCaseException;
import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import java.util.List;
import javax.json.JsonArray;
import javax.json.JsonReader;

/**
 * Reads {@link RequestCase} objects from an JSON document.
 *
 */
public class RequestCaseReader implements Closeable
  {  
  /**
   * Creates a new RequestCaseReader object.
   */
  public RequestCaseReader()
    {
    this( null);
    }
  
  /**
   * Creates a new RequestCaseReader object.
   */
  public RequestCaseReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a stream of {@link RequestCase} instances.
   */
  public List<RequestCase> getRequestCases()
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

      return RequestCaseJson.asRequestCases( json);
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

  public void close()
    {
    IOUtils.closeQuietly( getInputStream());
    }

  private InputStream stream_;
  }
