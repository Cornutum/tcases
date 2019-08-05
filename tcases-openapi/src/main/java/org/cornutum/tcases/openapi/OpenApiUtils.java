//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.SchemaUtils.*;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;

import java.util.Objects;
import java.util.Optional;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * Defines methods for accessing an OpenAPI specification.
 */
public final class OpenApiUtils
  {
  /**
   * Creates a new OpenApiUtils instance.
   */
  private OpenApiUtils()
    {
    // Static methods only
    }


  /**
   * Returns the given value if non-null. Otherwise, throws an exception.
   */
  public static <T> T expectedValueOf( T value, String description, Object... descriptionArgs)
    {
    if( value == null)
      {
      throw new IllegalStateException( String.format( description, descriptionArgs) + " is not defined");
      }

    return value;
    }

  /**
   * If the given parameter is defined by a reference, returns the referenced parameter. Otherwise, returns the given parameter.
   */
  public static Parameter resolveParameter( OpenAPI api, Parameter parameter)
    {
    return componentParameterRef( api, parameter.get$ref()).orElse( parameter);
    }

  /**
   * If the given schema is defined by a reference, returns the referenced schema. Otherwise, returns the given schema.
   */
  public static Schema<?> resolveSchema( OpenAPI api, Schema<?> schema)
    {
    return
      schema == null
      ? null
      : resolveSchemaType( componentSchemaRef( api, schema.get$ref()).orElse( schema));
    }

  /**
   * Returns the given schema after resolving schemas referenced by any "allOf", "anyOf", or "oneOf" members.
   */
  public static ComposedSchema resolveSchemaMembers( OpenAPI api, ComposedSchema composed)
    {
    // Resolve "allOf" schemas
    composed.setAllOf(
      Optional.ofNullable( composed.getAllOf()).orElse( emptyList())
      .stream()
      .map( member -> resolveSchema( api, member))
      .collect( toList()));
      
    // Resolve "anyOf" schemas
    composed.setAnyOf(
      Optional.ofNullable( composed.getAnyOf()).orElse( emptyList())
      .stream()
      .map( member -> resolveSchema( api, member))
      .collect( toList()));
      
    // Resolve "oneOf" schemas
    composed.setOneOf(
      Optional.ofNullable( composed.getOneOf()).orElse( emptyList())
      .stream()
      .map( member -> resolveSchema( api, member))
      .collect( toList()));

    return composed;
    }

  /**
   * If the given request body is defined by a reference, returns the referenced requestBody. Otherwise, returns the given request body.
   */
  public static RequestBody resolveRequestBody( OpenAPI api, RequestBody requestBody)
    {
    return componentRequestBodyRef( api, requestBody.get$ref()).orElse( requestBody);
    }

  /**
   * If the given response is defined by a reference, returns the referenced response. Otherwise, returns the given response.
   */
  public static ApiResponse resolveResponse( OpenAPI api, ApiResponse response)
    {
    return componentResponseRef( api, response.get$ref()).orElse( response);
    }

  /**
   * If the given header is defined by a reference, returns the referenced header. Otherwise, returns the given header.
   */
  public static Header resolveHeader( OpenAPI api, Header header)
    {
    return componentHeaderRef( api, header.get$ref()).orElse( header);
    }

  /**
   * When the given reference is non-null, returns the component parameter referenced.
   */
  public static Optional<Parameter> componentParameterRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( COMPONENTS_PARAMETERS_REF, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getParameters(), "Component parameters").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component schema referenced.
   */
  @SuppressWarnings("rawtypes")
  public static Optional<Schema> componentSchemaRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( COMPONENTS_SCHEMAS_REF, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getSchemas(), "Component schemas").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component request body referenced.
   */
  public static Optional<RequestBody> componentRequestBodyRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( COMPONENTS_REQUEST_BODIES_REF, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getRequestBodies(), "Component request bodies").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component response referenced.
   */
  public static Optional<ApiResponse> componentResponseRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( COMPONENTS_RESPONSES_REF, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getResponses(), "Component responses").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component header referenced.
   */
  public static Optional<Header> componentHeaderRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( COMPONENTS_HEADERS_REF, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getHeaders(), "Component headers").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * Returns the name of the given component reference.
   */
  public static String componentName( String refType, String ref)
    {
    return ref.startsWith( refType)? ref.substring( refType.length()) : null;
    }

  private static final String COMPONENTS_PARAMETERS_REF = "#/components/parameters/";
  private static final String COMPONENTS_REQUEST_BODIES_REF = "#/components/requestBodies/";
  private static final String COMPONENTS_RESPONSES_REF = "#/components/responses/";
  private static final String COMPONENTS_SCHEMAS_REF = "#/components/schemas/";
  private static final String COMPONENTS_HEADERS_REF = "#/components/headers/";
  }
