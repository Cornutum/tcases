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
import io.swagger.v3.oas.models.parameters.Parameter.StyleEnum;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import static io.swagger.v3.oas.models.parameters.Parameter.StyleEnum.*;

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
   * Returns the characters allowed in values for a parameter with the given properties.
   */
  public static Characters getParamCharacters( String location, String style)
    {
    Characters locationChars =
      "cookie".equals( location)
      ? Characters.COOKIE_VALUE
      : Characters.ANY;

    Character delimiter =
      "spaceDelimited".equals( style)?
      Character.valueOf( ' ') :

      "pipeDelimited".equals( style)?
      Character.valueOf( '|') :

      null;
    
    return
      Optional.ofNullable( delimiter)
      .map( d -> Characters.delimited( locationChars, d))
      .orElse( locationChars);
    }

  /**
   * If the given parameter is defined by a reference, returns the referenced parameter. Otherwise, returns the given parameter.
   */
  public static Parameter resolveParameter( OpenAPI api, Parameter parameter)
    {
    return
      Optional.ofNullable( parameter.get$ref())
      .map( ref -> componentParameterRef( api, ref))
      .orElse( parameter);
    }

  /**
   * If the given schema is defined by a reference, returns the referenced schema. Otherwise, returns the given schema.
   */
  public static Schema<?> resolveSchema( OpenAPI api, Schema<?> schema)
    {
    return
      schema == null?
      null :
      
      resolveSchemaType(
        Optional.ofNullable( schema.get$ref())
        .map( ref -> componentSchemaRef( api, ref))
        .orElse( schema));
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
    return
      Optional.ofNullable( requestBody.get$ref())
      .map( ref -> componentRequestBodyRef( api, ref))
      .orElse( requestBody);
    }

  /**
   * If the given response is defined by a reference, returns the referenced response. Otherwise, returns the given response.
   */
  public static ApiResponse resolveResponse( OpenAPI api, ApiResponse response)
    {
    return
      Optional.ofNullable( response.get$ref())
      .map( ref -> componentResponseRef( api, ref))
      .orElse( response);
    }

  /**
   * If the given header is defined by a reference, returns the referenced header. Otherwise, returns the given header.
   */
  public static Header resolveHeader( OpenAPI api, Header header)
    {
    return
      Optional.ofNullable( header.get$ref())
      .map( ref -> componentHeaderRef( api, ref))
      .orElse( header);
    }

  /**
   * When the given reference is non-null, returns the component parameter referenced.
   */
  public static Parameter componentParameterRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)
      .flatMap( ref -> Optional.ofNullable( componentName( COMPONENTS_PARAMETERS_REF, ref)))
      .flatMap( name -> Optional.ofNullable( expectedValueOf( expectedValueOf( api.getComponents(), "Components").getParameters(), "Component parameters").get( name)))
      .orElseThrow( () -> new IllegalStateException( String.format( "Can't resolve parameter reference=%s", reference)));
    }

  /**
   * When the given reference is non-null, returns the component schema referenced.
   */
  @SuppressWarnings("rawtypes")
  public static Schema componentSchemaRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)
      .flatMap( ref -> Optional.ofNullable( componentName( COMPONENTS_SCHEMAS_REF, ref)))
      .flatMap( name -> Optional.ofNullable( expectedValueOf( expectedValueOf( api.getComponents(), "Components").getSchemas(), "Component schemas").get( name)))
      .orElseThrow( () -> new IllegalStateException( String.format( "Can't resolve schema reference=%s", reference)));
    }

  /**
   * When the given reference is non-null, returns the component request body referenced.
   */
  public static RequestBody componentRequestBodyRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)
      .flatMap( ref -> Optional.ofNullable( componentName( COMPONENTS_REQUEST_BODIES_REF, ref)))
      .flatMap( name -> Optional.ofNullable( expectedValueOf( expectedValueOf( api.getComponents(), "Components").getRequestBodies(), "Component request bodies").get( name)))
      .orElseThrow( () -> new IllegalStateException( String.format( "Can't resolve request body reference=%s", reference)));
    }

  /**
   * When the given reference is non-null, returns the component response referenced.
   */
  public static ApiResponse componentResponseRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)
      .flatMap( ref -> Optional.ofNullable( componentName( COMPONENTS_RESPONSES_REF, ref)))
      .flatMap( name -> Optional.ofNullable( expectedValueOf( expectedValueOf( api.getComponents(), "Components").getResponses(), "Component responses").get( name)))
      .orElseThrow( () -> new IllegalStateException( String.format( "Can't resolve response reference=%s", reference)));
    }

  /**
   * When the given reference is non-null, returns the component header referenced.
   */
  public static Header componentHeaderRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)
      .flatMap( ref -> Optional.ofNullable( componentName( COMPONENTS_HEADERS_REF, ref)))
      .flatMap( name -> Optional.ofNullable( expectedValueOf( expectedValueOf( api.getComponents(), "Components").getHeaders(), "Component headers").get( name)))
      .orElseThrow( () -> new IllegalStateException( String.format( "Can't resolve header reference=%s", reference)));
    }

  /**
   * Returns the name of the given component reference.
   */
  public static String componentName( String refType, String ref)
    {
    return ref.startsWith( refType)? ref.substring( refType.length()) : null;
    }

  /**
   * Returns the given parameter "style" property if it is applicable for the given
   * parameter "in" and "type" properties. Otherwise, throws an {@link InvalidStyleException}.
   */
  public static String ifApplicableStyle( String style, String in, String type) throws InvalidStyleException
    {
    return String.valueOf( ifApplicableStyle( StyleEnum.valueOf( style.toUpperCase()), in, type));
    }

  /**
   * Returns the given parameter "style" property if it is applicable for the given
   * parameter "in" and "type" properties. Otherwise, throws an {@link InvalidStyleException}.
   */
  public static StyleEnum ifApplicableStyle( StyleEnum style, String in, String type) throws InvalidStyleException
    {
    StyleEnum specified = style;
    StyleEnum applicable;

    type = Optional.ofNullable( type).orElse( "null");

    if( "query".equals( in) ||  "cookie".equals( in))
      {
      switch( specified)
        {
        case FORM:
          {
          applicable = specified;
          break;
          }
        case DEEPOBJECT:
          {
          if( "object".equals( type) || "null".equals( type))
            {
            applicable = specified;
            }
          else
            {
            applicable = FORM;
            throw
              new InvalidStyleException(
                String.format( "style=%s is not applicable for parameter type=%s", specified, type),
                applicable.toString());
            }
          break;
          }
        case PIPEDELIMITED:
        case SPACEDELIMITED:
          {
          if( !( "array".equals( type) || "null".equals( type)))
            {
            applicable = FORM;
            throw
              new InvalidStyleException(
                String.format( "style=%s is not applicable for parameter type=%s", specified, type),
                applicable.toString());
            }
          else if( specified == SPACEDELIMITED && "cookie".equals( in))
            {
            applicable = FORM;
            throw
              new InvalidStyleException(
                String.format( "style=%s is not applicable for a %s parameter", specified, in),
                applicable.toString());
            }
          else
            {
            applicable = specified;
            }
            
          break;
          }
        default:
          {
          applicable = FORM;
          throw
            new InvalidStyleException(
              String.format( "style=%s is not applicable for a %s parameter", specified, in),
              applicable.toString());
          }
        }
      }
    
    else if( "path".equals( in))
      {
      switch( specified)
        {
        case SIMPLE:
        case MATRIX:
        case LABEL:
          {
          applicable = specified;
          break;
          }
        default:
          {
          applicable = SIMPLE;
          throw
            new InvalidStyleException(
              String.format( "style=%s is not applicable for a %s parameter", specified, in),
              applicable.toString());
          }
        }
      }
    
    else if( "header".equals( in))
      {
      switch( specified)
        {
        case SIMPLE:
          {
          applicable = specified;
          break;
          }
        default:
          {
          applicable = SIMPLE;
          throw
            new InvalidStyleException(
              String.format( "style=%s is not applicable for a %s parameter", specified, in),
              applicable.toString());
          }
        }
      }
    
    else
      {
      throw new OpenApiException( String.format( "'%s' is not a valid parameter location", in)); 
      }

    return applicable;
    }

  private static final String COMPONENTS_PARAMETERS_REF = "#/components/parameters/";
  private static final String COMPONENTS_REQUEST_BODIES_REF = "#/components/requestBodies/";
  private static final String COMPONENTS_RESPONSES_REF = "#/components/responses/";
  private static final String COMPONENTS_SCHEMAS_REF = "#/components/schemas/";
  private static final String COMPONENTS_HEADERS_REF = "#/components/headers/";
  }
