/////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi.io;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.TcasesJson;
import org.cornutum.tcases.openapi.ModelOptions;
import org.cornutum.tcases.openapi.TcasesOpenApi;
import org.cornutum.tcases.openapi.reader.OpenApiReader;
import org.cornutum.tcases.openapi.resolver.RequestCases;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.resolver.ResolverContext;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefWriter;
import org.cornutum.tcases.openapi.test.ResponsesDef;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Defines methods for generating Tcases documents from OpenAPI definitions.
 * <P/>
 * OpenAPI definitions must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public final class TcasesOpenApiIO
  {
  /**
   * Creates a new TcasesOpenApiIO instance.
   */
  private TcasesOpenApiIO()
    {
    // Static methods only
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( InputStream api)
    {
    return getRequestInputModel( api, null);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( InputStream api, ModelOptions options)
    {
    return getRequestInputModel( api, null, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI definition document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( InputStream api, String docType, ModelOptions options)
    {
    try( OpenApiReader reader = new OpenApiReader( api, docType))
      {
      return TcasesOpenApi.getRequestInputModel( reader.read(), options);
      }
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( File api)
    {
    return getRequestInputModel( api, null);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( File api, ModelOptions options)
    {
    return getRequestInputModel( api, null, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( File api, String defaultDocType, ModelOptions options)
    {
    try( OpenApiReader reader = new OpenApiReader( api, defaultDocType))
      {
      return TcasesOpenApi.getRequestInputModel( reader.read(), options);
      }
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API requests defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemTestDef getRequestTests( InputStream api)
    {
    return getRequestTests( api, null);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API requests defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemTestDef getRequestTests( InputStream api, ModelOptions options)
    {
    return getRequestTests( api, null, options);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  public static SystemTestDef getRequestTests( InputStream api, String docType, ModelOptions options)
    {
    return Tcases.getTests( getRequestInputModel( api, docType, options), null, null);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API requests defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemTestDef getRequestTests( File api)
    {
    return getRequestTests( api, null);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API requests defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API requests to model.
   */
  public static SystemTestDef getRequestTests( File api, ModelOptions options)
    {
    return getRequestTests( api, null, options);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  public static SystemTestDef getRequestTests( File api, String defaultDocType, ModelOptions options)
    {
    return Tcases.getTests( getRequestInputModel( api, defaultDocType, options), null, null);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemInputDef getResponseInputModel( InputStream api)
    {
    return getResponseInputModel( api, null);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemInputDef getResponseInputModel( InputStream api, ModelOptions options)
    {
    return getResponseInputModel( api, null, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the given
   * OpenAPI definition document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemInputDef getResponseInputModel( InputStream api, String docType, ModelOptions options)
    {
    try( OpenApiReader reader = new OpenApiReader( api, docType))
      {
      return TcasesOpenApi.getResponseInputModel( reader.read(), options);
      }
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
   */
  public static SystemInputDef getResponseInputModel( File api)
    {
    return getResponseInputModel( api, null);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
   */
  public static SystemInputDef getResponseInputModel( File api, ModelOptions options)
    {
    return getResponseInputModel( api, null, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
   */
  public static SystemInputDef getResponseInputModel( File api, String defaultDocType, ModelOptions options)
    {
    try( OpenApiReader reader = new OpenApiReader( api, defaultDocType))
      {
      return TcasesOpenApi.getResponseInputModel( reader.read(), options);
      }
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API responses defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemTestDef getResponseTests( InputStream api)
    {
    return getResponseTests( api, null);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API responses defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemTestDef getResponseTests( InputStream api, ModelOptions options)
    {
    return getResponseTests( api, null, options);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API responses defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
   */
  public static SystemTestDef getResponseTests( InputStream api, String docType, ModelOptions options)
    {
    return Tcases.getTests( getResponseInputModel( api, docType, options), null, null);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API responses defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemTestDef getResponseTests( File api)
    {
    return getResponseTests( api, null);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API responses defined by the 
   * OpenAPI definition in the given JSON document. Returns null if the given definition defines no API responses to model.
   */
  public static SystemTestDef getResponseTests( File api, ModelOptions options)
    {
    return getResponseTests( api, null, options);
    }

  /**
   * Returns a {@link SystemTestDef system test definition} for the API responses defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
   */
  public static SystemTestDef getResponseTests( File api, String defaultDocType, ModelOptions options)
    {
    return Tcases.getTests( getResponseInputModel( api, defaultDocType, options), null, null);
    }

  /**
   * Returns a {@link ResponsesDef request responses definition} for the given OpenAPI definition.
   */
  public static ResponsesDef getResponsesDef( File api)
    {
    return null;
    }

  /**
   * Writes a {@link org.cornutum.tcases.io.SystemInputJsonWriter JSON document} describing the given system input definition to the given output stream.
   */
  public static void writeInputModel( SystemInputDef inputDef, OutputStream outputStream)
    {
    TcasesJson.writeInputModel( inputDef, outputStream);
    }

  /**
   * Writes a {@link org.cornutum.tcases.io.SystemTestJsonWriter JSON document} describing the given test case definitions to the given output stream.
   */
  public static void writeTests( SystemTestDef testDef, OutputStream outputStream)
    {
    TcasesJson.writeTests( testDef, outputStream);
    }

  /**
   * Writes a {@link org.cornutum.tcases.openapi.resolver.io.RequestTestDefWriter JSON document} listing test cases defined by the given
   * request test case definitions to the given output stream.
   */
  public static void writeRequestCases( SystemTestDef testDef, ResolverContext context, OutputStream outputStream)
    {
    writeRequestCases( RequestCases.getRequestCases( testDef, context), outputStream);
    }

  /**
   * Writes a {@link org.cornutum.tcases.openapi.resolver.io.RequestTestDefWriter JSON document} listing the given request test cases
   * to the given output stream.
   */
  public static void writeRequestCases( RequestTestDef requestTestDef, OutputStream outputStream)
    {
    try( RequestTestDefWriter writer = new RequestTestDefWriter( outputStream))
      {
      writer.write( requestTestDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write request test cases", e);
      }
    }
  }
