/////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.ICondition;
import org.cornutum.tcases.util.ListBuilder;
import static org.cornutum.tcases.DefUtils.toIdentifier;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.servers.Server;

import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.math.RoundingMode.DOWN;
import static java.math.RoundingMode.UP;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Converts between OpenAPI models and Tcases input models.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public class InputModeller
  {
  /**
   * Creates a new InputModeller instance.
   */
  public InputModeller()
    {
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI specification. Returns null if the given spec defines no API requests to model.
   */
  public SystemInputDef getRequestInputModel( OpenAPI api)
    {
    Info info;
    String title;
    try
      {
      info = expectedValueOf( api.getInfo(), "API info");
      title = expectedValueOf( StringUtils.trimToNull( info.getTitle()), "API title");
      }
    catch( Exception e)
      {
      throw new OpenApiException( "Invalid API spec", e);
      }

    return
      with( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "version", info.getVersion())
          .hasIf( "server", membersOf( api.getServers()).findFirst().map( Server::getUrl))
          .functions( entriesOf( api.getPaths()).flatMap( path -> pathRequestDefs( api, path.getKey(), path.getValue())))
          .build();

        return
          inputDef.getFunctionInputDefs().hasNext()
          ? inputDef
          : null;
        });
    }

  /**
   * Returns a request {@link FunctionInputDef function input definition} for each of the API operations for the given path.
   */
  private Stream<FunctionInputDef> pathRequestDefs( OpenAPI api, String path, PathItem pathItem)
    {
    return
      with( path,

      () ->
      Stream.of(
        opRequestDef( api, path, pathItem, "GET", pathItem.getGet()),
        opRequestDef( api, path, pathItem, "PUT", pathItem.getPut()),
        opRequestDef( api, path, pathItem, "POST", pathItem.getPost()),
        opRequestDef( api, path, pathItem, "DELETE", pathItem.getDelete()),
        opRequestDef( api, path, pathItem, "OPTIONS", pathItem.getOptions()),
        opRequestDef( api, path, pathItem, "HEAD", pathItem.getHead()),
        opRequestDef( api, path, pathItem, "PATCH", pathItem.getPatch()),
        opRequestDef( api, path, pathItem, "TRACE", pathItem.getTrace()))

      // Skip if operation not defined
      .filter( Objects::nonNull)

      // Skip if operation has no inputs to model
      .filter( functionDef -> functionDef.getVarDefs().hasNext()));
    }

  /**
   * Returns the request {@link FunctionInputDef function input definition} for the given API operation.
   */
  private FunctionInputDef opRequestDef( OpenAPI api, String path, PathItem pathItem, String opName, Operation op)
    {
    return
      with( opName,
        () ->
        op == null?
        null :

        FunctionInputDefBuilder.with( String.format( "%s_%s", opName, functionPathName( path)))
        .hasIf( "server", membersOf( pathItem.getServers()).findFirst().map( Server::getUrl))
        .hasIf( "server", membersOf( op.getServers()).findFirst().map( Server::getUrl))
        .vars( opParameters( pathItem, op).map( p -> parameterVarDef( api, resolveParameter( api, p))))
        .vars( iterableOf( requestBodyVarDef( api, op.getRequestBody())))
        .build());
    }

  /**
   * Returns the consolidated set of parameters for the given API operation.
   */
  private Stream<Parameter> opParameters( PathItem pathItem, Operation op)
    {
    return
      Stream.concat( membersOf( pathItem.getParameters()), membersOf( op.getParameters()))
      .collect( toMap( Parameter::getName, Function.identity(), (pathParam, opParam) -> opParam, () -> new LinkedHashMap<String,Parameter>()))
      .values()
      .stream();
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the given
   * OpenAPI specification. Returns null if the given spec defines no API responses to model.
   */
  public SystemInputDef getResponseInputModel( OpenAPI api)
    {
    Info info;
    String title;
    try
      {
      info = expectedValueOf( api.getInfo(), "API info");
      title = expectedValueOf( StringUtils.trimToNull( info.getTitle()), "API title");
      }
    catch( Exception e)
      {
      throw new OpenApiException( "Invalid API spec", e);
      }

    return
      with( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "version", info.getVersion())
          .hasIf( "server", membersOf( api.getServers()).findFirst().map( Server::getUrl))
          .functions( entriesOf( api.getPaths()).flatMap( path -> pathResponseDefs( api, path.getKey(), path.getValue())))
          .build();

        return
          inputDef.getFunctionInputDefs().hasNext()
          ? inputDef
          : null;
        });
    }

  /**
   * Returns a response {@link FunctionInputDef function input definition} for each of the API operations for the given path.
   */
  private Stream<FunctionInputDef> pathResponseDefs( OpenAPI api, String path, PathItem pathItem)
    {
    return
      with( path,

      () ->
      Stream.of(
        opResponseDef( api, path, pathItem, "GET", pathItem.getGet()),
        opResponseDef( api, path, pathItem, "PUT", pathItem.getPut()),
        opResponseDef( api, path, pathItem, "POST", pathItem.getPost()),
        opResponseDef( api, path, pathItem, "DELETE", pathItem.getDelete()),
        opResponseDef( api, path, pathItem, "OPTIONS", pathItem.getOptions()),
        opResponseDef( api, path, pathItem, "HEAD", pathItem.getHead()),
        opResponseDef( api, path, pathItem, "PATCH", pathItem.getPatch()),
        opResponseDef( api, path, pathItem, "TRACE", pathItem.getTrace()))

      // Skip if operation not defined
      .filter( Objects::nonNull)

      // Skip if operation has no inputs to model
      .filter( functionDef -> functionDef.getVarDefs().hasNext()));
    }

  /**
   * Returns the response {@link FunctionInputDef function input definition} for the given API operation.
   */
  private FunctionInputDef opResponseDef( OpenAPI api, String path, PathItem pathItem, String opName, Operation op)
    {
    return
      with( opName,
        () ->
        op == null?
        null :

        FunctionInputDefBuilder.with( String.format( "%s_%s", opName, functionPathName( path)))
        .hasIf( "server", membersOf( pathItem.getServers()).findFirst().map( Server::getUrl))
        .hasIf( "server", membersOf( op.getServers()).findFirst().map( Server::getUrl))
        .vars( responsesVars( api, expectedValueOf( op.getResponses(), "responses")))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given request body.
   */
  private Optional<IVarDef> requestBodyVarDef( OpenAPI api, RequestBody body)
    {
    return
      with( "requestBody",
        () ->
        Optional.ofNullable( body)
        .map( b -> resolveRequestBody( api, b))
        .map( b -> {
          String instanceVarName = "Body";
          Map<String,MediaType> mediaTypes = expectedValueOf( b.getContent(), "Request body content");
          return
            VarSetBuilder.with( instanceVarName)
            .type( "request")
            .members(
              instanceDefinedVar( instanceVarName, Boolean.TRUE.equals( b.getRequired())),
              mediaTypeVar( instanceVarName, mediaTypes))
            .members(
              mediaTypeContentVars( api, mediaTypes))
            .build();
          }));
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given media types.
   */
  private IVarDef mediaTypeVar( String instanceVarTag, Map<String,MediaType> mediaTypes)
    {
    return
      VarDefBuilder.with( "Media-Type")
      .when( instanceDefinedCondition( instanceVarTag))
      .values(
        mediaTypes.entrySet().stream()
        .map(
          contentDef ->
          {
          String mediaType = contentDef.getKey();
          String mediaTypeVarName = mediaTypeVarName( mediaType);
          return
            VarValueDefBuilder.with( mediaTypeVarName)
            .has( "mediaType", mediaType)
            .properties( mediaTypeVarName)
            .build();
          })) 
      .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).build())
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given media type content.
   */
  private Stream<IVarDef> mediaTypeContentVars( OpenAPI api, Map<String,MediaType> mediaTypes)
    {
    return
      mediaTypes.entrySet().stream()
      .map(
        contentDef ->
        {
        String mediaType = contentDef.getKey();

        return
          with( mediaType,
            () ->
            {
            String mediaTypeVarName = mediaTypeVarName( mediaType);
            VarSetBuilder contentVar =
              VarSetBuilder.with( mediaTypeVarName)
              .when( has( mediaTypeVarName));

            // Schema defined for this media type?
            Schema<?> mediaTypeSchema = contentDef.getValue().getSchema();
            if( mediaTypeSchema == null)
              {
              // No, use perfunctory "must be defined" input model for contents
              contentVar.members( instanceDefinedVar( mediaTypeVarName, false));
              }
            else
              {
              // Yes, use schema input model for contents
              contentVar.members( instanceSchemaVars( api, mediaTypeVarName, false, resolveSchema( api, mediaTypeSchema)));
              }

            return contentVar.build();
            });
        });
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given parameter.
   */
  private IVarDef parameterVarDef( OpenAPI api, Parameter parameter)
    {
    return
      with( parameter.getName(),
        () ->
        {
        String parameterVarName = toIdentifier( parameter.getName());
        String parameterIn = expectedValueOf( parameter.getIn(), "in", parameter.getName());
        Schema<?> parameterSchema = parameterSchema( api, parameter);
        String parameterType = parameterSchema.getType();

        return
          VarSetBuilder.with( parameterVarName)
          .type( parameterIn)
          .members( parameterDefinedVar( parameterVarName, parameterType, parameter))
          .members( instanceSchemaVars( api, parameterVarName, parameterSchema))
          .build();
        });
    }

  /**
   * Returns the schema for the given parameter.
   */
  private Schema<?> parameterSchema( OpenAPI api, Parameter parameter)
    {
    // The schema should be defined either by...
    Schema<?> schema =
      // ... the schema property...
      Optional.ofNullable( parameter.getSchema())
      
      // ... or the content property
      .orElse(
        Optional.ofNullable( parameter.getContent())
        .map( content -> content.values().stream().findFirst().map( MediaType::getSchema).orElse( null))
        .orElse( null));

    return
      // Schema defined?
      schema == null?

      // No, assume parameter is a plain string
      new StringSchema() :
      
      // Yes, resolve any reference to another schema definition
      resolveSchema( api, schema);
    }

  /**
   * Returns the style of the given parameter.
   */
  private Parameter.StyleEnum parameterStyle( Parameter parameter)
    {
    String in = parameter.getIn();
    Parameter.StyleEnum style = parameter.getStyle();
    
    return
      style != null?
      style :

      in.equals( "path")?
      Parameter.StyleEnum.SIMPLE :

      in.equals( "header")?
      Parameter.StyleEnum.SIMPLE :

      Parameter.StyleEnum.FORM;
    }

  /**
   * Resolves the value of a parameter explode property.
   */
  private Boolean parameterExplode( Boolean explode, String parameterType, Parameter.StyleEnum parameterStyle)
    {
    return
      !("object".equals( parameterType) || "array".equals( parameterType))?
      null :

      explode != null?
      explode :

      parameterStyle == Parameter.StyleEnum.FORM?
      true :

      false;
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given parameter is defined.
   */
  private IVarDef parameterDefinedVar( String parameterVarTag, String parameterType, Parameter parameter)
    {
    try
      {
      Parameter.StyleEnum parameterStyle = parameterStyle( parameter);
      
      return
        VarDefBuilder.with( instanceDefinedVar( parameterVarTag, Boolean.TRUE.equals( parameter.getRequired())))
        .has( "style", parameterStyle)
        .has( "explode", parameterExplode( parameter.getExplode(), parameterType, parameterStyle))
        .build();
      }
    catch( Exception e)
      {
      throw new OpenApiException( String.format( "Can't create 'Defined' variable for parameter=%s", parameter.getName()), e);
      }
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given responses.
   */
  private Stream<IVarDef> responsesVars( OpenAPI api, ApiResponses responses)
    {
    return
      with( "responses",
        () ->
        Stream.concat(
          Stream.of(
            VarDefBuilder.with( "Status-Code")
            .type( "response")
            .values( responseStatusValues( api, responses))
            .build()),

          responseVars( api, responses)));
    }

  /**
   * Returns the value definitions for the response status code variable.
   */
  private Stream<VarValueDef> responseStatusValues( OpenAPI api, ApiResponses responses)
    {
    return
      Stream.concat(
        // One for each specified status code...
        responses.keySet().stream()
        .filter( status -> !status.equals( "default"))
        .map( status -> VarValueDefBuilder.with( status).properties( statusCodeProperty( status)).build()),

        // And one for any unspecified status code...
        Stream.of( VarValueDefBuilder.with( "Other").properties( statusCodeProperty( "Other")).build()));
    }

  /**
   * Returns the variable definitions for the given response definitions.
   */
  private Stream<IVarDef> responseVars( OpenAPI api, ApiResponses responses)
    {
    return
      responses.keySet().stream()
      .map( status -> {
        return
          with(
            status,
            () ->  {
            ApiResponse response = resolveResponse( api, responses.get( status));
            String statusValueName = status.equals( "default")? "Other" : status;
            return
              VarSetBuilder.with( statusValueName)
              .when( has( statusCodeProperty( statusValueName)))
              .type( "response")
              .members( iterableOf( responseHeadersVar( api, status, response)))
              .members( responseContentVar( api, status, response))
              .build();
            });
        });
    }

  /**
   * Returns the variable definition(s) for headers in the given response.
   */
  private Optional<IVarDef> responseHeadersVar( OpenAPI api, String status, ApiResponse response)
    {
    return
      with( "headers",
        () -> {
        List<String> headerNames =
          Optional.ofNullable( response.getHeaders())
          .map( headers -> headers.keySet().stream().filter( name -> !name.equals( "Content-Type")).collect( toList()))
          .orElse( emptyList());

        return
          Optional.ofNullable( headerNames.isEmpty()? null : headerNames)
          .map( names -> {
            return
              VarSetBuilder.with( "Headers")
              .members(
                names.stream()
                .map( name -> responseHeaderVar( api, name, resolveHeader( api, response.getHeaders().get( name)))))
              .build();
            });
        });
    }

  /**
   * Returns the variable definition for the given response header.
   */
  private IVarDef responseHeaderVar( OpenAPI api, String headerName, Header header)
    {
    return
      with(
        headerName,
        () -> {
        String headerVarName = toIdentifier( headerName);
        String headerVarTag = "header" + StringUtils.capitalize( headerVarName);

        Schema<?> headerSchema =
          header.getSchema() == null
          ? new StringSchema()
          : resolveSchema( api, header.getSchema());

        return
          VarSetBuilder.with( headerVarName)
          .members( headerDefinedVar( headerVarTag, header))
          .members( instanceSchemaVars( api, headerVarTag, headerSchema))
          .build();
        });
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given header is defined.
   */
  private IVarDef headerDefinedVar( String headerVarTag, Header header)
    {
    return
      VarDefBuilder.with( instanceDefinedVar( headerVarTag, header.getRequired()))
      .has( "style", header.getStyle())
      .has( "explode", header.getExplode())
      .build();
    }

  /**
   * Returns the variable definition(s) for the content of the given response.
   */
  private IVarDef responseContentVar( OpenAPI api, String status, ApiResponse response)
    {
    String contentVarName = "Content";
    return
      with( "content",
        () ->
        Optional.ofNullable( response.getContent())
        .map( mediaTypes -> {
          return
            VarSetBuilder.with( contentVarName)
            .members(
              instanceDefinedVar( contentVarName, true),
              mediaTypeVar( contentVarName, mediaTypes))
            .members(
              mediaTypeContentVars( api, mediaTypes))
            .build();
          })

        .orElse(
          VarSetBuilder.with( contentVarName)
          .members( instanceUndefinedAlways( contentVarName)) 
          .build()));
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given instance is defined.
   */
  private VarDef instanceDefinedVar( String instanceVarTag, boolean required, String... propertiesWhenDefined)
    {
    return instanceDefinedVar( instanceVarTag, null, required, propertiesWhenDefined);
    }

  /**
   * Returns an {@link IVarDef input variable} to represent that the given instance is always undefined.
   */
  private VarDef instanceUndefinedAlways( String instanceVarTag)
    {
    return instanceDefinedVar( instanceVarTag, Boolean.FALSE, false);
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given instance is defined.
   */
  private VarDef instanceDefinedVar( String instanceVarTag, Boolean forceDefined, boolean required, String... propertiesWhenDefined)
    {
    VarDefBuilder varDef = VarDefBuilder.with( "Defined");

    if( !Boolean.FALSE.equals( forceDefined))
      {
      varDef
        .values(
          VarValueDefBuilder.with( "Yes")
          .properties( instanceDefinedProperty( instanceVarTag))
          .properties( propertiesWhenDefined)
          .build());
      }

    if( !Boolean.TRUE.equals( forceDefined))
      {
      varDef
        .values(
          VarValueDefBuilder.with( "No")
          .type( forceDefined == null && required? VarValueDef.Type.FAILURE : VarValueDef.Type.VALID)
          .build());
      }

    return varDef.build();
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> instanceSchemaVars( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    return instanceSchemaVars( api, instanceVarTag, true, instanceSchema);      
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> instanceSchemaVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    ComposedSchema composedSchema = instanceSchema instanceof ComposedSchema? (ComposedSchema) instanceSchema : null;
    String type = composedSchema == null? expectedValueOf( instanceSchema.getType(), "Schema type") : null;

    return
      composedSchema != null?
      composedSchemaVars( api, instanceVarTag, instanceOptional, composedSchema) :
      
      "string".equals( type)?
      instanceStringVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "integer".equals( type)?
      instanceIntegerVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "boolean".equals( type)?
      instanceBooleanVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "number".equals( type)?
      instanceNumberVars( api, instanceVarTag, instanceOptional, instanceSchema) :

      "array".equals( type)?
      instanceArrayVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "object".equals( type)?
      instanceObjectVars( api, instanceVarTag, instanceOptional, instanceSchema) :

      Stream.empty();      
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given array instance.
   */
  private Stream<IVarDef> instanceArrayVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    // Handle Swagger parser defect, which creates an ArraySchema iff the "items" property is specified, regardless of the value of "type".
    // See https://github.com/swagger-api/swagger-parser/issues/1047.
    ArraySchema arraySchema =
      expectedValueOf(
        instanceSchema instanceof ArraySchema? (ArraySchema) instanceSchema : null,
        "Array items schema");
    
    Schema<?> itemSchema = resolveSchema( api, arraySchema.getItems());
    
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),

        VarSetBuilder.with( "Items")
        .when( has( instanceValueProperty( instanceVarTag)))
        .members(
          arraySizeVar( api, instanceVarTag, arraySchema),
          arrayItemsVar( api, instanceVarTag, itemSchema))
        .members(
          iterableOf( arrayUniqueItemsVar( api, instanceVarTag, arraySchema)))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the size of an array instance.
   */
  private IVarDef arraySizeVar( OpenAPI api, String instanceVarTag, ArraySchema arraySchema)
    {
    // Arrays size constrained?
    VarDefBuilder size = VarDefBuilder.with( "Size");
    Integer minItems = arraySchema.getMinItems();
    Integer maxItems = arraySchema.getMaxItems();
    if( minItems == null && maxItems == null)
      {
      // No, add standard boundary condition values
      size.values(
        VarValueDefBuilder.with( 0).build(),
        VarValueDefBuilder.with( 1).properties( arrayItemsProperty( instanceVarTag)).build(),
        VarValueDefBuilder.with( "> 1").properties( arrayItemsProperty( instanceVarTag), arrayItemsManyProperty( instanceVarTag)).build());
      }
    else
      {
      // Ensure min/max range is feasible
      minItems =
        Optional.ofNullable( minItems)
        .map( min -> Optional.ofNullable( maxItems).map( max -> Math.min( min, max)).orElse( min))
        .orElse( null);

      // Add min/max boundary condition values
      TreeSet<Integer> sizeValues = new TreeSet<Integer>();
      if( minItems != null)
        {
        sizeValues.add( minItems - 1);
        sizeValues.add( minItems);
        }
      if( maxItems != null)
        {
        sizeValues.add( maxItems);
        sizeValues.add( maxItems + 1);
        }

      for( Integer sizeValue : sizeValues)
        {
        if( sizeValue >= 0)
          {
          VarValueDefBuilder sizeBuilder = VarValueDefBuilder.with( sizeValue);
          if( (minItems != null && sizeValue < minItems) || (maxItems != null && sizeValue > maxItems))
            {
            sizeBuilder.type( VarValueDef.Type.FAILURE);
            }
          else
            {
            sizeBuilder
              .properties( Optional.ofNullable( sizeValue > 0? arrayItemsProperty( instanceVarTag) : null))
              .properties( Optional.ofNullable( sizeValue > 1? arrayItemsManyProperty( instanceVarTag) : null));
            }
          size.values( sizeBuilder.build());
          }
        }

      if( maxItems == null)
        {
        int many = Math.max( 1, minItems);
        size.values(
          VarValueDefBuilder.with( String.format( "> %s", many))
          .properties( arrayItemsProperty( instanceVarTag), arrayItemsManyProperty( instanceVarTag))
          .build());
        }
      else if( minItems == null)
        {
        size.values( VarValueDefBuilder.with( 0).build());
        if( maxItems > 1)
          {
          size.values(
            VarValueDefBuilder.with( String.format( "< %s", maxItems))
            .properties( arrayItemsProperty( instanceVarTag))
            .properties( Optional.ofNullable( maxItems > 2? arrayItemsManyProperty( instanceVarTag) : null))
            .build());
          }
        }
      }

    return size.build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the items of the given array instance.
   */
  private IVarDef arrayItemsVar( OpenAPI api, String instanceVarTag, Schema<?> itemSchema)
    {
    return
      with( "items",
        () ->
        VarSetBuilder.with( "Contains")
        .when( has( arrayItemsProperty( instanceVarTag)))
        .members( instanceSchemaVars( api, arrayItemsProperty( instanceVarTag), false, itemSchema))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the unique items property of an array instance.
   */
  private Optional<IVarDef> arrayUniqueItemsVar( OpenAPI api, String instanceVarTag, ArraySchema arraySchema)
    {
    boolean uniqueRequired = Boolean.TRUE.equals( arraySchema.getUniqueItems());

    return
      Optional.ofNullable( arraySchema.getMaxItems()).orElse( Integer.MAX_VALUE) <= 1

      ? Optional.empty()

      : Optional.of(
          VarDefBuilder.with( "Unique")
          .when( has( arrayItemsManyProperty( instanceVarTag)))
          .values(
            VarValueDefBuilder.with( "Yes").build(),
            VarValueDefBuilder.with( "No").type( uniqueRequired? VarValueDef.Type.FAILURE: VarValueDef.Type.VALID).build())
          .build());
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given boolean instance.
   */
  private Stream<IVarDef> instanceBooleanVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        booleanValueVar( api, instanceVarTag, instanceSchema));
    }    

  /**
   * Returns the {@link IVarDef input variables} defined by the given integer instance.
   */
  private Stream<IVarDef> instanceIntegerVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        integerValueVar( api, instanceVarTag, instanceSchema));
    }   

  /**
   * Returns the {@link IVarDef input variables} defined by the given number instance.
   */
  private Stream<IVarDef> instanceNumberVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        numberValueVar( api, instanceVarTag, instanceSchema));
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given object instance.
   */
  private Stream<IVarDef> instanceObjectVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        objectValueVar( api, instanceVarTag, instanceSchema));
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given string instance.
   */
  private Stream<IVarDef> instanceStringVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        stringValueVar( api, instanceVarTag, instanceSchema));
    }   

  /**
   * Returns the {@link IVarDef input variables} defined by the given composed schema.
   */
  private Stream<IVarDef> composedSchemaVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, ComposedSchema composedSchema)
    {
    return
      Stream.of(
        Optional.ofNullable( composedSchema.getAllOf())
        .map( memberSchemas -> allOfVar( api, instanceVarTag, instanceOptional, memberSchemas))
        .orElse( null),
        
        Optional.ofNullable( composedSchema.getAnyOf())
        .map( memberSchemas -> anyOfVar( api, instanceVarTag, instanceOptional, memberSchemas))
        .orElse( null),

        Optional.ofNullable( composedSchema.getOneOf())
        .map( memberSchemas -> oneOfVar( api, instanceVarTag, instanceOptional, memberSchemas))
        .orElse( null))

      .filter( Objects::nonNull);
    } 

  /**
   * Returns the {@link IVarDef input variable} defined by the given "allOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef allOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, List<Schema> memberSchemas)
    {
    String containerType = "AllOf";
    String containerVarTag = instanceVarTag + containerType;
    
    return
      VarSetBuilder.with( containerType)
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .members(
        VarSetBuilder.with( "Members")
        .members(
          IntStream.range( 0, memberSchemas.size())
          .mapToObj( i -> {
            return
              with( String.format( "allOf[%s]", i),
                () ->
                {
                String memberVarTag = containerVarTag + i;
                return
                  VarSetBuilder.with( String.valueOf(i))
                  .members( memberSchemaVars( api, containerVarTag, memberVarTag, resolveSchema( api, memberSchemas.get(i)), true))
                  .build();
                });
            }))
        .build())
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} defined by the given "anyOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef anyOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, List<Schema> memberSchemas)
    {
    return oneOfVar( api, instanceVarTag, instanceOptional, "anyOf", memberSchemas);
    } 

  /**
   * Returns the {@link IVarDef input variable} defined by the given "oneOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef oneOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, List<Schema> memberSchemas)
    {
    return oneOfVar( api, instanceVarTag, instanceOptional, "oneOf", memberSchemas);
    } 

  /**
   * Returns the {@link IVarDef input variable} defined by the given "oneOf" or "anyOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef oneOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, String containerType, List<Schema> memberSchemas)
    {
    String containerVarName = StringUtils.capitalize( containerType);
    String containerVarTag = instanceVarTag + containerVarName;
    boolean validationRequired = memberSchemas.size() == 1;
    boolean anyOf = containerType.equals( "anyOf");
    
    return
      VarSetBuilder.with( containerVarName)
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .members(
        VarSetBuilder.with( "Members")
        .members(
          iterableOf( oneOfValidatedVar( containerVarTag, validationRequired, anyOf)))
        .members(
          IntStream.range( 0, memberSchemas.size())
          .mapToObj( i -> {
            return
              with( String.format( "%s[%s]", containerType, i),
                () ->
                {
                String memberVarTag = containerVarTag + i;
                return
                  VarSetBuilder.with( String.valueOf(i))
                  .members( memberSchemaVars( api, containerVarTag, memberVarTag, resolveSchema( api, memberSchemas.get(i)), validationRequired))
                  .build();
                });
            }))
        .build())
      .build();
    } 

  /**
   * Returns the {@link IVarDef input variable} representing the "validated" state of a "oneOf" (or "anyOf") schema.
   */
  private Optional<IVarDef> oneOfValidatedVar( String containerVarTag, boolean validationRequired, boolean anyOf)
    {
    return
      Optional.of( memberValidatedProperty( containerVarTag))
      .filter( membersValidated -> !validationRequired)
      .map( membersValidated -> {
        return
          VarDefBuilder.with( "Validated")
          .values(
            VarValueDefBuilder.with( "1")
            .when( equalTo( membersValidated, 1))
            .build(), 

            VarValueDefBuilder.with( "0")
            .when( not( membersValidated))
            .type( VarValueDef.Type.FAILURE)
            .build(),

            VarValueDefBuilder.with( "> 1")
            .when( moreThan( membersValidated, 1))
            .type( anyOf? VarValueDef.Type.ONCE : VarValueDef.Type.FAILURE)
            .build())
          .build();
        });
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given member schema.
   */
  private Stream<IVarDef> memberSchemaVars( OpenAPI api, String containerVarTag, String memberVarTag, Schema<?> memberSchema, boolean validationRequired)
    {
    // Starting with all variables defined by the member schema, update to provide for a single validation failure
    List<IVarDef> memberSchemaVars = instanceSchemaVars( api, memberVarTag, false, memberSchema).collect( toList());
    if(
      // Is this member schema a basic type (not composed)?
      memberSchemaVars.size() > 1
      ||
      // Or is it a member of an anyOf/oneOf schema?
      !validationRequired)
      {
      // Yes, find all descendant variables that define a failure value...
      List<VarDef> failureVars =
        toStream( new VarDefIterator( memberSchemaVars.iterator()))
        .filter( v -> v.getFailureValues().hasNext())
        .collect( toList());

      if( !failureVars.isEmpty())
        {
        // ... and for most of them, remove all failure values...
        int last = failureVars.size() - 1;
        failureVars.subList( 0, last).stream()
          .forEach( v -> {
            List<VarValueDef> failures = toStream( v.getFailureValues()).collect( toList());
            failures.stream().forEach( failure -> v.removeValue( failure.getName()));
            });

        // ... but for one descendant variable, select a single failure value to represent the member validation failure case.
        makeMemberFailure( failureVars.get( last), containerVarTag, validationRequired);
        }
      }
        
    return memberSchemaVars.stream();
    }

  /**
   * Update the given variable definition to represent the member failure case for the specified container variable.
   */
  private void makeMemberFailure( VarDef failureVar, String containerVarTag, boolean validationRequired)
    {
    VarValueDef selectedFailure = null;
    for( VarValueDef value : toStream( failureVar.getValues()).collect( toList()))
      {
      if( value.getType() != VarValueDef.Type.FAILURE)
        {
        // Non-failure values must indicate the "member valid" state
        if( !validationRequired)
          {
          value.addProperties( memberValidatedProperty( containerVarTag));
          }
        }

      else if( selectedFailure == null)
        {
        // One failure value must become the sole alternative for the "member not valid" state
        selectedFailure = value;
        if( !validationRequired)
          {
          value.setType( VarValueDef.Type.VALID);
          }
        }

      else
        {
        // And all other failure values must be removed
        failureVar.removeValue( value.getName());
        }
      }
    }

  /**
   * Returns the {@link IVarDef input variable} representing the type of a instance.
   */
  private IVarDef instanceTypeVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    String type = expectedValueOf( instanceSchema.getType(), "type");
    Boolean nullable = instanceSchema.getNullable();

    return
      VarDefBuilder.with( "Type")
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .values(
        VarValueDefBuilder.with( type).properties( instanceValueProperty( instanceVarTag)).build(),
        VarValueDefBuilder.with( (Object) null).type( Boolean.TRUE.equals( nullable)? VarValueDef.Type.ONCE : VarValueDef.Type.FAILURE).build(),
        VarValueDefBuilder.with( String.format( "Not %s", type)).type( VarValueDef.Type.FAILURE).build())
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an integer instance.
   */
  @SuppressWarnings("unchecked")
  private IVarDef integerValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    VarDefBuilder value =
      VarDefBuilder.with( "Value")
      .has( "format", instanceSchema.getFormat())
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .when( has( instanceValueProperty( instanceVarTag)));

    // Enumerated values?
    Schema<Number> numberSchema = (Schema<Number>) instanceSchema;
    List<Number> enums = Optional.ofNullable( numberSchema.getEnum()).orElse( emptyList());
    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      value.values( enums.stream().map( i -> VarValueDefBuilder.with( i).build()));
      value.values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).build());
      }
    else
      {
      // No, add min/max boundary condition values
      if( instanceSchema.getMultipleOf() != null)
        {
        value.values( VarValueDefBuilder.with( String.format( "Not a multiple of %s", instanceSchema.getMultipleOf())).type( VarValueDef.Type.FAILURE).build());
        }

      if( instanceSchema.getMinimum() == null && instanceSchema.getMaximum() == null)
        {
        // Value is unconstrained -- add standard boundary conditions
        value.values(
          VarValueDefBuilder.with( "< 0").build(),
          VarValueDefBuilder.with( 0).build(),
          VarValueDefBuilder.with( "> 0").build());
        }
      else
        {      
        int multipleOf =
          Optional.ofNullable( instanceSchema.getMultipleOf())
          .map( BigDecimal::intValue)
          .orElse( 1);
        
        Integer minimum =
          Optional.ofNullable( instanceSchema.getMinimum())
          .map( BigDecimal::intValue)
          .map( i -> Boolean.TRUE.equals( instanceSchema.getExclusiveMinimum())? ((Math.floorDiv( i, multipleOf) + 1) * multipleOf) : i)
          .orElse( null);

        Integer maximum =
          Optional.ofNullable( instanceSchema.getMaximum())
          .map( BigDecimal::intValue)
          .map( i -> Boolean.TRUE.equals( instanceSchema.getExclusiveMaximum())? ((((int) Math.ceil( (double)i/multipleOf)) - 1) * multipleOf) : i)
          .orElse( null);

        TreeSet<Integer> boundaryValues = new TreeSet<Integer>();
        if( minimum != null)
          {
          boundaryValues.add( minimum);
          boundaryValues.add( minimum - multipleOf);
          }
        if( maximum != null)
          {
          boundaryValues.add( maximum);
          boundaryValues.add( maximum + multipleOf);
          }
        for( Integer i : boundaryValues)
          {
          value.values
            ( VarValueDefBuilder.with( i)
              .type(
                (minimum != null && i < minimum) || (maximum != null && i > maximum)
                ? VarValueDef.Type.FAILURE
                : VarValueDef.Type.VALID)
              .build());
          }
        if( minimum == null)
          {
          value.values( VarValueDefBuilder.with( String.format( "< %s", maximum)).build());
          }
        else if( maximum == null)
          {
          value.values( VarValueDefBuilder.with( String.format( "> %s", minimum)).build());
          }
        }
      }

    return value.build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a number instance.
   */
  private IVarDef numberValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    VarDefBuilder value =
      VarDefBuilder.with( "Value")
      .has( "format", instanceSchema.getFormat())
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .when( has( instanceValueProperty( instanceVarTag)));

    // Enumerated values?
    NumberSchema numberSchema = (NumberSchema) instanceSchema;
    List<BigDecimal> enums = Optional.ofNullable( numberSchema.getEnum()).orElse( emptyList());
    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      value.values( enums.stream().map( i -> VarValueDefBuilder.with( i).build()));
      value.values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).build());
      }
    else
      {
      // No, add min/max boundary condition values
      if( instanceSchema.getMultipleOf() != null)
        {
        value.values( VarValueDefBuilder.with( String.format( "Not a multiple of %s", instanceSchema.getMultipleOf())).type( VarValueDef.Type.FAILURE).build());
        }

      if( instanceSchema.getMinimum() == null && instanceSchema.getMaximum() == null)
        {
        // Value is unconstrained -- add standard boundary conditions
        value.values(
          VarValueDefBuilder.with( "< 0").build(),
          VarValueDefBuilder.with( 0).build(),
          VarValueDefBuilder.with( "> 0").build());
        }
      else
        {
        BigDecimal multipleOf =
          Optional.ofNullable( instanceSchema.getMultipleOf())
          .orElse(
            new BigDecimal(
              BigInteger.ONE,
              Math.max(
                Optional.ofNullable( instanceSchema.getMinimum()).map( BigDecimal::scale).orElse( 0),
                Optional.ofNullable( instanceSchema.getMaximum()).map( BigDecimal::scale).orElse( 0))));

        BigDecimal minimum =
          Optional.ofNullable( instanceSchema.getMinimum())
          .map( m -> Boolean.TRUE.equals( instanceSchema.getExclusiveMinimum())? m.divide( multipleOf, 0, DOWN).add( BigDecimal.ONE).multiply( multipleOf) : m)
          .orElse( null);

        BigDecimal maximum =
          Optional.ofNullable( instanceSchema.getMaximum())
          .map( m -> Boolean.TRUE.equals( instanceSchema.getExclusiveMaximum())? m.divide( multipleOf, 0, UP).subtract( BigDecimal.ONE).multiply( multipleOf) : m)
          .orElse( null);

        TreeSet<BigDecimal> boundaryValues = new TreeSet<BigDecimal>();
        if( minimum != null)
          {
          boundaryValues.add( minimum);
          boundaryValues.add( minimum.subtract( multipleOf));
          }
        if( maximum != null)
          {
          boundaryValues.add( maximum);
          boundaryValues.add( maximum.add( multipleOf));
          }
        for( BigDecimal n : boundaryValues)
          {
          value.values
            ( VarValueDefBuilder.with( n)
              .type(
                (minimum != null && n.compareTo(minimum) < 0) || (maximum != null && n.compareTo(maximum) > 0)
                ? VarValueDef.Type.FAILURE
                : VarValueDef.Type.VALID)
              .build());
          }
        if( minimum == null)
          {
          value.values( VarValueDefBuilder.with( String.format( "< %s", maximum)).build());
          }
        else if( maximum == null)
          {
          value.values( VarValueDefBuilder.with( String.format( "> %s", minimum)).build());
          }
        }
      }

    return value.build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an object instance.
   */
  private IVarDef objectValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    return
      VarSetBuilder.with( "Value")
      .when( has( instanceValueProperty( instanceVarTag)))
      .members( iterableOf( objectPropertyCountVar( api, instanceVarTag, instanceSchema))) 
      .members( objectPropertiesVar( api, instanceVarTag, instanceSchema))
      .build();
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the number of properties for an object instance.
   */
  private Optional<IVarDef> objectPropertyCountVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    // Accumulate property count values depending on constraints
    ListBuilder<VarValueDef> countValues = ListBuilder.to();
    
    int requiredCount = Optional.ofNullable( instanceSchema.getRequired()).map( List::size).orElse( 0);
    int totalCount = Optional.ofNullable( instanceSchema.getProperties()).map( Map::size).orElse( 0);

    boolean hasAdditional =
      Optional.ofNullable( instanceSchema.getAdditionalProperties())
      .map( additional -> additional.getClass().equals( Boolean.class)? (Boolean) additional : true)
      .orElse( false);

    Integer minProperties =
      Optional.ofNullable( instanceSchema.getMinProperties())
      .filter( min -> min > requiredCount && (hasAdditional || min < totalCount))
      .orElse( null);

    Integer maxProperties =
      Optional.ofNullable( instanceSchema.getMaxProperties())
      .filter( max -> max > requiredCount && (hasAdditional || max < totalCount))
      .orElse( null);
    
    // Property count constrained (ignoring infeasible and superfluous constraints)?
    if( !(minProperties == null && maxProperties == null))
      {
      // Yes, upper bound defined?
      if( maxProperties != null)
        {
        // Yes, with provision for any lower bound...
        if( minProperties != null)
          {
          countValues
            .add(
              VarValueDefBuilder.with( String.format( "< %s", minProperties))
              .when( lessThan( objectPropertiesProperty( instanceVarTag), minProperties)) 
              .type( VarValueDef.Type.FAILURE)
              .build());
          }
        else if( requiredCount == 0)
          {
          countValues.add(
            VarValueDefBuilder.with( 0)
            .when( not( objectPropertiesProperty( instanceVarTag))) 
            .type( VarValueDef.Type.ONCE)
            .build());
          }

        // ... and keeping in mind that maxProperties can exceed totalCount when additional properties are allowed...
        Optional<Integer> maxDefined =
          Optional.ofNullable(
            maxProperties > totalCount
            ? totalCount + 1
            : null);
          
        // ...define coverage w.r.t. upper bound
        countValues
          .add(
            VarValueDefBuilder.with( String.format( "%s%s", maxProperties.equals( minProperties)? "" : "<= ", maxProperties))
            .when( notMoreThan( objectPropertiesProperty( instanceVarTag), maxDefined.orElse( maxProperties)))
            .build())
          .add(
            VarValueDefBuilder.with( String.format( "> %s", maxProperties))
            .when( moreThan( objectPropertiesProperty( instanceVarTag), maxDefined.map( md -> md - 1).orElse( maxProperties)))
            .type( VarValueDef.Type.FAILURE)
            .build());
        }
      else
        {
        // No, define coverage w.r.t. lower bound    
        boolean requiresAdditional = hasAdditional && minProperties > totalCount;

        // If additional properties are required to meet the minimum, use the "additional" variable to
        // define the "below minimum" failure condition
        countValues
          .add(
            VarValueDefBuilder.with( String.format( ">= %s", minProperties))
            .when( notLessThan( objectPropertiesProperty( instanceVarTag), requiresAdditional? totalCount : minProperties))
            .build());

        if( !requiresAdditional)
          {
          countValues
            .add(
              VarValueDefBuilder.with( String.format( "< %s", minProperties))
              .when( lessThan( objectPropertiesProperty( instanceVarTag), minProperties)) 
              .type( VarValueDef.Type.FAILURE)
              .build());
          }
        }
      }

    // No, but are all properties optional?
    else if( requiredCount == 0 && totalCount > 0)
      {
      // Yes, ensure coverage of "no properties" case.
      countValues
        .add(
          VarValueDefBuilder.with( 0)
          .when( not( objectPropertiesProperty( instanceVarTag))) 
          .type( VarValueDef.Type.ONCE)
          .build())
        .add(
          VarValueDefBuilder.with( "> 0")
          .when( has( objectPropertiesProperty( instanceVarTag))) 
          .type( VarValueDef.Type.VALID)
          .build());
      }

    return
      Optional.of( countValues.build())
      .filter( values -> !values.isEmpty())
      .map( values -> 
            VarDefBuilder.with( "Property-Count")
            .values( values)
            .build());
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the properties of an object instance.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef objectPropertiesVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    Map<String,Schema> propertyDefs = Optional.ofNullable( instanceSchema.getProperties()).orElse( emptyMap());
    List<String> requiredProperties = Optional.ofNullable( instanceSchema.getRequired()).orElse( emptyList());

    return
      VarSetBuilder.with( "Properties")

      .members(
        propertyDefs.entrySet().stream()
        .map(
          propertyDef ->
          objectPropertyVar(
            api,
            instanceVarTag,
            propertyDef.getKey(),
            requiredProperties.contains( propertyDef.getKey()),
            resolveSchema( api, propertyDef.getValue()))))

      .members(
        objectAdditionalVar( api, instanceVarTag, instanceSchema))

      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an object instance property.
   */
  private IVarDef objectPropertyVar( OpenAPI api, String instanceVarTag, String propertyName, boolean required, Schema<?> propertySchema)
    {
    return
      with( propertyName,
        () ->
        {
        String propertyVarName = toIdentifier( propertyName);
        String propertyVarTag = instanceVarTag + StringUtils.capitalize( propertyVarName);
      
        return
          VarSetBuilder.with( propertyVarName)
          .members( instanceDefinedVar( propertyVarTag, required, objectPropertiesProperty( instanceVarTag)))
          .members( instanceSchemaVars( api, propertyVarTag, propertySchema))
          .build();
        });
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the additional properties of an object instance.
   */
  private IVarDef objectAdditionalVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    Class<?> type =
      Optional.ofNullable( instanceSchema.getAdditionalProperties())
      .map( Object::getClass)
      .orElse( null);

    Schema<?> propertySchema =
      type != null && !type.equals( Boolean.class)
      ? (Schema<?>)instanceSchema.getAdditionalProperties()
      : null;

    boolean allowed =
      type != null
      &&
      (propertySchema != null || Boolean.TRUE.equals((Boolean) instanceSchema.getAdditionalProperties()));

    boolean required =
      allowed
      &&
      Optional.ofNullable( instanceSchema.getMinProperties())
      .map( min -> min > Optional.ofNullable( instanceSchema.getProperties()).map( Map::size).orElse( 0))
      .orElse( false);
        
    return
      propertySchema == null ?

      VarDefBuilder.with( "Additional")
      .values(
        VarValueDefBuilder.with( "Yes")
        .type( allowed? VarValueDef.Type.VALID : VarValueDef.Type.FAILURE)
        .properties( Optional.ofNullable( allowed? objectPropertiesProperty( instanceVarTag) : null))
        .build(),

        VarValueDefBuilder.with( "No")
        .type( required? VarValueDef.Type.FAILURE : VarValueDef.Type.VALID)
        .build())
      .build() :

      objectPropertyVar( api, instanceVarTag, "Additional", required, propertySchema);
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a string instance.
   */
  private IVarDef stringValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    IVarDef valueVar;

    // Enumerated values?
    String format = instanceSchema.getFormat();
    List<FormattedString> enums =
      Optional.ofNullable( instanceSchema.getEnum())
      .map( e -> FormattedString.of( format, e))
      .orElse( emptyList());
    
    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      valueVar = 
        VarDefBuilder.with( "Value")
        .has( "format", format)
        .has( "default", Objects.toString( FormattedString.of( format, instanceSchema.getDefault()), null))
        .when( has( instanceValueProperty( instanceVarTag)))
        .values( enums.stream().map( i -> VarValueDefBuilder.with( String.valueOf( i)).build()))
        .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).build())
        .build();
      }
    else
      {
      // No, add inputs for other string assertions.
      VarSetBuilder valueVarSet =
        VarSetBuilder.with( "Value")
        .has( "format", format)
        .has( "default", FormattedString.of( format, instanceSchema.getDefault()))
        .when( has( instanceValueProperty( instanceVarTag)));

      // Add values for any length assertions
      VarDefBuilder length = VarDefBuilder.with( "Length");
      Integer minLength = instanceSchema.getMinLength();
      Integer maxLength = instanceSchema.getMaxLength();

      // Any length assertions specified?
      if( minLength == null && maxLength == null)
        {
        // No, add standard values
        length.values( VarValueDefBuilder.with( "> 0").build());

        // Length constrained by format?
        if( format == null || format.equals( "byte") || format.equals( "binary") || format.equals( "password"))
          {
          // No, allow empty values
          length.values( VarValueDefBuilder.with( 0).build());
          }
        }
      else
        {
        // Yes, add boundary condition values
        TreeSet<Integer> boundaryValues = new TreeSet<Integer>();
        if( minLength != null)
          {
          boundaryValues.add( minLength);
          boundaryValues.add( minLength - 1);
          }
        if( maxLength != null)
          {
          boundaryValues.add( maxLength);
          boundaryValues.add( maxLength + 1);
          }
        for( Integer i : boundaryValues)
          {
          if( i >= 0)
            {
            length.values
              ( VarValueDefBuilder.with( i)
                .type(
                  (minLength != null && i < minLength) || (maxLength != null && i > maxLength)
                  ? VarValueDef.Type.FAILURE
                  : VarValueDef.Type.VALID)
                .build());
            }
          }
        if( minLength == null)
          {
          length.values( VarValueDefBuilder.with( 0).build());
          if( maxLength > 1)
            {
            length.values( VarValueDefBuilder.with( String.format( "< %s", maxLength)).build());
            }
          }
        else if( maxLength == null)
          {
          length.values( VarValueDefBuilder.with( String.format( "> %s", minLength)).build());
          }
        }
      valueVarSet.members( length.build());

      // Add values for any pattern assertion
      Optional.ofNullable( instanceSchema.getPattern())
        .map(
          pattern ->
          VarDefBuilder.with( "MatchesPattern")
          .has( "pattern", pattern)
          .values(
            VarValueDefBuilder.with( "Yes").build(),
            VarValueDefBuilder.with( "No").type( VarValueDef.Type.FAILURE).build())
          .build())
        .ifPresent( varDef -> valueVarSet.members( varDef));

      // Complete inputs for all string assertions
      valueVar = valueVarSet.build();
      }

    return valueVar;
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a boolean instance.
   */
  private IVarDef booleanValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    BooleanSchema booleanSchema = (BooleanSchema) instanceSchema;
    List<Boolean> possibleValues = Arrays.asList( Boolean.TRUE, Boolean.FALSE);
    List<Boolean> allowedValues = Optional.ofNullable( booleanSchema.getEnum()).orElse( possibleValues);

    return
      VarDefBuilder.with( "Value")
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .when( has( instanceValueProperty( instanceVarTag)))
      .values(
        possibleValues.stream()
        .filter( Objects::nonNull)
        .map( b -> {
          return
            VarValueDefBuilder.with( b)
            .type( allowedValues.contains(b)? VarValueDef.Type.VALID : VarValueDef.Type.FAILURE)
            .build();
          }))
      .build();
    }

  /**
   * Returns the component of a function name that represents the given API request path.
   */
  private String functionPathName( String pathName)
    {
    return
      Arrays.stream( pathName.split( "/"))
      .filter( p -> !p.isEmpty())
      .map( p -> toIdentifier( p))
      .collect( joining( "-"));
    }

  /**
   * Returns input variable name for the given media type
   */
  private String mediaTypeVarName( String mediaType)
    {
    return functionPathName( mediaType);
    }

  /**
   * Returns "defined" condition for the given instance.
   */
  private ICondition instanceDefinedCondition( String instanceTag, boolean instanceOptional)
    {
    return
      instanceOptional
      ? has( instanceDefinedProperty( instanceTag))
      : null;
    }

  /**
   * Returns "defined" condition for the given instance.
   */
  private ICondition instanceDefinedCondition( String instanceTag)
    {
    return instanceDefinedCondition( instanceTag, true);
    }

  /**
   * Returns the "defined" property for the given instance.
   */
  private String instanceDefinedProperty( String instanceTag)
    {
    return instanceTag;
    }

  /**
   * Returns the "has value" property for the given instance.
   */
  private String instanceValueProperty( String instanceTag)
    {
    return instanceTag + "Value";
    }

  /**
   * Returns the "has items" property for the given array instance.
   */
  private String arrayItemsProperty( String instanceTag)
    {
    return instanceTag + "Items";
    }

  /**
   * Returns the "has many items" property for the given array instance.
   */
  private String arrayItemsManyProperty( String instanceTag)
    {
    return arrayItemsProperty( instanceTag) + "Many";
    }

  /**
   * Returns the "has properties" property for the given object instance.
   */
  private String objectPropertiesProperty( String instanceTag)
    {
    return instanceTag + "Properties";
    }

  /**
   * Returns the "member validated" property for the given composed schema instance.
   */
  private String memberValidatedProperty( String instanceTag)
    {
    return instanceTag + "MemberValidated";
    }

  /**
   * Returns the "has status code" property for the given status.
   */
  private String statusCodeProperty( String status)
    {
    return "status" + StringUtils.capitalize( status);
    }

  /**
   * Returns the given value if non-null. Otherwise, throws an OpenApiException.
   */
  private <T> T expectedValueOf( T value, String description, Object... descriptionArgs)
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
  private Parameter resolveParameter( OpenAPI api, Parameter parameter)
    {
    return componentParameterRef( api, parameter.get$ref()).orElse( parameter);
    }

  /**
   * If the given schema is defined by a reference, returns the referenced schema. Otherwise, returns the given schema.
   */
  private Schema<?> resolveSchema( OpenAPI api, Schema<?> schema)
    {
    return componentSchemaRef( api, schema.get$ref()).orElse( schema);
    }

  /**
   * If the given request body is defined by a reference, returns the referenced requestBody. Otherwise, returns the given request body.
   */
  private RequestBody resolveRequestBody( OpenAPI api, RequestBody requestBody)
    {
    return componentRequestBodyRef( api, requestBody.get$ref()).orElse( requestBody);
    }

  /**
   * If the given response is defined by a reference, returns the referenced response. Otherwise, returns the given response.
   */
  private ApiResponse resolveResponse( OpenAPI api, ApiResponse response)
    {
    return componentResponseRef( api, response.get$ref()).orElse( response);
    }

  /**
   * If the given header is defined by a reference, returns the referenced header. Otherwise, returns the given header.
   */
  private Header resolveHeader( OpenAPI api, Header header)
    {
    return componentHeaderRef( api, header.get$ref()).orElse( header);
    }

  /**
   * When the given reference is non-null, returns the component parameter referenced.
   */
  private Optional<Parameter> componentParameterRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( componentsParametersRef_, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getParameters(), "Component parameters").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component schema referenced.
   */
  @SuppressWarnings("rawtypes")
  private Optional<Schema> componentSchemaRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( componentsSchemasRef_, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getSchemas(), "Component schemas").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component request body referenced.
   */
  private Optional<RequestBody> componentRequestBodyRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( componentsRequestBodiesRef_, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getRequestBodies(), "Component request bodies").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component response referenced.
   */
  private Optional<ApiResponse> componentResponseRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( componentsResponsesRef_, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getResponses(), "Component responses").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * When the given reference is non-null, returns the component header referenced.
   */
  private Optional<Header> componentHeaderRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( componentsHeadersRef_, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getHeaders(), "Component headers").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * Returns the name of the given component reference.
   */
  private String componentName( String refType, String ref)
    {
    return ref.startsWith( refType)? ref.substring( refType.length()) : null;
    }

  /**
   * Returns the result of the given supplier with the specified context.
   */
  private <T> T with( String context, Supplier<T> supplier)
    {
    context_.addLast( context);
    try
      {
      return supplier.get();
      }
    catch( OpenApiException oae)
      {
      throw oae;
      }
    catch( Exception e)
      {
      String location = toStream( context_.iterator()).collect( joining( ", "));
      throw new OpenApiException( String.format( "Error processing %s", location), e);
      }
    finally
      {
      context_.removeLast();
      }
    }

  private Deque<String> context_ = new ArrayDeque<String>();
  
  private static final String componentsParametersRef_ = "#/components/parameters/";
  private static final String componentsRequestBodiesRef_ = "#/components/requestBodies/";
  private static final String componentsResponsesRef_ = "#/components/responses/";
  private static final String componentsSchemasRef_ = "#/components/schemas/";
  private static final String componentsHeadersRef_ = "#/components/headers/";
}
