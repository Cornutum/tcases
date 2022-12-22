//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonSchemaResolver;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * Reads a {@link Project} definition from an JSON document.
 *
 */
public class ProjectJsonReader implements Closeable
  {  
  /**
   * Creates a new ProjectJsonReader object.
   */
  public ProjectJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new ProjectJsonReader object.
   */
  public ProjectJsonReader( InputStream stream)
    {
    converter_ = new ProjectJson( LoggerFactory.getLogger( ProjectJsonReader.class));
    setInputStream( stream);
    }

  /**
   * Returns a {@link Project} instance.
   */
  public Project getProject()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    
    JsonSchema schema = 
      service.createSchemaReaderFactoryBuilder().withSchemaResolver( new SchemaResourceResolver( service)).build()
      .createSchemaReader( openSchemaResource( "project-schema.json"))
      .read();
    
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
        throw new ProjectException( "Invalid project definition", e);
        }

      return converter_.asProject( json);
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

  /**
   * Returns an InputStream to read the given schema resource.
   */
  private static InputStream openSchemaResource( String resourceName)
    {
    return ProjectJsonReader.class.getResourceAsStream( "/schema/" + resourceName);
    }

  private final ProjectJson converter_;
  private InputStream stream_;

  /**
   * Resolves references to schema resources.
   */
  private static class SchemaResourceResolver implements JsonSchemaResolver
    {
    /**
     * Creates a new SchemaResourceResolver instance.
     */
    public SchemaResourceResolver( JsonValidationService service)
      {
      service_ = service;
      }

    @Override
    public JsonSchema resolveSchema( URI id)
      {
      return service_.readSchema( openSchemaResource( new File( id.getPath()).getName()));
      }
    
    private final JsonValidationService service_;
    }
  }
