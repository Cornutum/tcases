//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.reader;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.ObjectMapperFactory;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import io.swagger.v3.parser.util.OpenAPIDeserializer;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.Objects;
import java.util.Optional;
import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Reads an {@link OpenAPI} object from an Open API Version 3 document.
 */
public class OpenApiReader implements Closeable
  {
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader()
    {
    this( (InputStream) null);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( InputStream input)
    {
    this( input, null, null);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( InputStream input, String docType)
    {
    this( input, null, docType);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( InputStream input, URL location)
    {
    this( input, location, null);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( InputStream input, URL location, String docType)
    {
    this( input == null? null : readerFor( input), location, docType);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( File input)
    {
    this( input, null);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( File input, String docType)
    {
    this( inputFor( input), toUrl( input), docType);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( Reader input)
    {
    this( input, null, null);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( Reader input, String docType)
    {
    this( input, null, docType);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( Reader input, URL location)
    {
    this( input, location, null);
    }
  
  /**
   * Creates a new OpenApiReader instance.
   */
  public OpenApiReader( Reader input, URL location, String docType)
    {
    reader_ = input;
    location_ = location;

    docType_ =
      Optional.ofNullable( StringUtils.trimToNull( docType))
      .map( String::toLowerCase)
      .orElse( docTypeFor( location));
    }

  /**
   * Returns the location of this document.
   */
  public URL getLocation()
    {
    return location_;
    }

  /**
   * Returns the content type of this document.
   */
  public String getDocType()
    {
    return docType_;
    }

  /**
   * Returns the {@link OpenAPI} instance represented by this document.
   */
  public OpenAPI read()
    {
    JsonNode json;
    
    try
      {
      ObjectMapper mapper =
        isYaml()
        ? ObjectMapperFactory.createYaml()
        : ObjectMapperFactory.createJson();
    
      json = mapper.readTree( getReader());
      }
    catch( Exception e)
      {
      throw new OpenApiReaderException( getLocation(), e);
      }

    SwaggerParseResult parseResult= new OpenAPIDeserializer().deserialize( json, Objects.toString( getLocation(), null));

    Optional.of( parseResult)
      .map( SwaggerParseResult::getMessages)
      .filter( messages -> !messages.isEmpty())
      .map( messages -> new OpenApiReaderException( getLocation(), messages))
      .ifPresent( failure -> { throw failure; });

    return parseResult.getOpenAPI();
    }

  public void close()
    {
    IOUtils.closeQuietly( reader_);
    }

  /**
   * Returns the Reader for this document.
   */
  private Reader getReader()
    {
    return
      reader_ != null
      ? reader_
      : readerFor( System.in);
    }

  /**
   * Returns true if this is a YAML document.
   */
  private boolean isYaml()
    {
    String docType = getDocType();
    return "yml".equals( docType) || "yaml".equals( docType);
    }

  /**
   * Returns a URL for the given file.
   */
  private static URL toUrl( File file)
    {
    try
      {
      return
        file == null
        ? null
        : file.toURI().toURL();
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( "Can't get URL for file=" + file);
      }
    }

  /**
   * Returns an InputStream for the given file.
   */
  private static InputStream inputFor( File file)
    {
    try
      {
      return
        file == null
        ? null
        : new FileInputStream( file);
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( "Can't open file=" + file);
      }
    }

  /**
   * Returns a Reader for the given input stream.
   */
  private static Reader readerFor( InputStream input)
    {
    return new InputStreamReader( input, UTF_8);
    }

  /**
   * Returns the content type defined by the given document location.
   */
  private static String docTypeFor( URL location)
    {
    return
      Optional.ofNullable( location)
      .flatMap( url -> Optional.ofNullable( StringUtils.trimToNull( FilenameUtils.getExtension( url.getPath()))))
      .orElse( "json");
    }

  private Reader reader_;
  private URL location_;
  private String docType_;
  }
