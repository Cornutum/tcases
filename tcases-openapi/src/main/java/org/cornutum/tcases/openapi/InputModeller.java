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
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.servers.Server;

import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
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
import static java.util.stream.Collectors.toSet;

/**
 * Converts between OpenAPI models and Tcases input models.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public abstract class InputModeller
  {
  protected enum View { REQUEST, RESPONSE };
  
  /**
   * Creates a new InputModeller instance.
   */
  protected InputModeller( View view)
    {
    this( view, null);
    }
  
  /**
   * Creates a new InputModeller instance.
   */
  protected InputModeller( View view, ModelOptions options)
    {
    view_ = expectedValueOf( view, "Model view");
    setOptions( Optional.ofNullable( options).orElse( new ModelOptions()));
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI specification. Returns null if the given spec defines no API requests to model.
   */
  protected SystemInputDef requestInputModel( OpenAPI api)
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
      .filter( this::hasInputs));
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
  protected SystemInputDef responseInputModel( OpenAPI api)
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
      .filter( this::hasInputs));
    }

  /**
   * Returns if the given {@link FunctionInputDef function input definition} defines input variables.
   */
  private boolean hasInputs( FunctionInputDef functionDef)
    {
    boolean hasVars = functionDef.getVarDefs().hasNext();
    if( !hasVars)
      {
      notifyWarning( String.format( "No inputs to model for operation=%s", functionDef.getName()));
      }
    
    return hasVars;
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
          String contentVarTag = "Content";
          Map<String,MediaType> mediaTypes = expectedValueOf( b.getContent(), "Request body content");
          return
            VarSetBuilder.with( "Body")
            .type( "request")
            .members(
              instanceDefinedVar( contentVarTag, Boolean.TRUE.equals( b.getRequired())),
              mediaTypeVar( contentVarTag, mediaTypes))
            .members(
              mediaTypeContentVars( api, contentVarTag, mediaTypes))
            .build();
          }));
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given media types.
   */
  private IVarDef mediaTypeVar( String contentVarTag, Map<String,MediaType> mediaTypes)
    {
    return
      VarDefBuilder.with( "Media-Type")
      .when( instanceDefinedCondition( contentVarTag))
      .values(
        mediaTypes.entrySet().stream()
        .map(
          contentDef ->
          {
          String mediaType = contentDef.getKey();
          String mediaTypeVarName = mediaTypeVarName( mediaType);
          String mediaTypeVarTag = mediaTypeVarTag( contentVarTag, mediaType);
          return
            VarValueDefBuilder.with( mediaTypeVarName)
            .has( "mediaType", mediaType)
            .properties( mediaTypeVarTag)
            .build();
          })) 
      .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).build())
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given media type content.
   */
  private Stream<IVarDef> mediaTypeContentVars( OpenAPI api, String contentVarTag, Map<String,MediaType> mediaTypes)
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
            String mediaTypeVarTag = mediaTypeVarTag( contentVarTag, mediaType);
            
            VarSetBuilder contentVar =
              VarSetBuilder.with( mediaTypeVarName)
              .when( has( mediaTypeVarTag));

            // Schema defined for this media type?
            Schema<?> mediaTypeSchema = contentDef.getValue().getSchema();
            if( mediaTypeSchema == null)
              {
              // No, use perfunctory "must be defined" input model for contents
              notifyWarning( String.format( "No schema defined for media type=%s", mediaType));
              contentVar.members( instanceDefinedVar( mediaTypeVarTag, false));
              }
            else
              {
              // Yes, use schema input model for contents
              contentVar.members( instanceSchemaVars( api, mediaTypeVarTag, false, resolveSchema( api, mediaTypeSchema)));
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

    // Resolve any reference to another schema definition
    return resolveSchema( api, schema);
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
    Parameter.StyleEnum parameterStyle = parameterStyle( parameter);
      
    return
      VarDefBuilder.with( instanceDefinedVar( parameterVarTag, Boolean.TRUE.equals( parameter.getRequired())))
      .has( "style", parameterStyle)
      .has( "explode", parameterExplode( parameter.getExplode(), parameterType, parameterStyle))
      .build();
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
                .map( name -> responseHeaderVar( api, status, name, resolveHeader( api, response.getHeaders().get( name)))))
              .build();
            });
        });
    }

  /**
   * Returns the variable definition for the given response header.
   */
  private IVarDef responseHeaderVar( OpenAPI api, String status, String headerName, Header header)
    {
    return
      with(
        headerName,
        () -> {
        String headerVarName = toIdentifier( headerName);
        String headerVarTag = status + "Header" + StringUtils.capitalize( headerVarName);
        Schema<?> headerSchema = resolveSchema( api, header.getSchema());

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
      VarDefBuilder.with( instanceDefinedVar( headerVarTag, Boolean.TRUE.equals( header.getRequired())))
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
    String contentVarTag = status + contentVarName;
    return
      with( "content",
        () ->
        Optional.ofNullable( response.getContent())
        .map( mediaTypes -> {
          return
            VarSetBuilder.with( contentVarName)
            .members(
              instanceDefinedVar( contentVarTag, true),
              mediaTypeVar( contentVarTag, mediaTypes))
            .members(
              mediaTypeContentVars( api, contentVarTag, mediaTypes))
            .build();
          })

        .orElse(
          VarSetBuilder.with( contentVarName)
          .members( instanceDefinedNever( contentVarTag)) 
          .build()));
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given instance is defined.
   */
  private VarDef instanceDefinedVar( String instanceVarTag, boolean required, String... propertiesWhenDefined)
    {
    return
      instanceDefinedVar(
        instanceVarTag,
        required? Definition.REQUIRED : Definition.OPTIONAL,
        propertiesWhenDefined);
    }

  /**
   * Returns an {@link IVarDef input variable} to represent that the given instance is always undefined.
   */
  private VarDef instanceDefinedNever( String instanceVarTag)
    {
    return instanceDefinedVar( instanceVarTag, Definition.NEVER);
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given instance is defined.
   */
  private VarDef instanceDefinedVar( String instanceVarTag, Definition definition, String... propertiesWhenDefined)
    {
    VarDefBuilder varDef = VarDefBuilder.with( "Defined");

    if( definition != Definition.NEVER)
      {
      VarValueDefBuilder yes = VarValueDefBuilder.with( "Yes");
      if( definition == Definition.EXCLUDED)
        {
        yes.type( VarValueDef.Type.FAILURE);
        }
      else
        {
        yes
          .properties( instanceDefinedProperty( instanceVarTag))
          .properties( propertiesWhenDefined);
        }
      varDef.values( yes.build());
      }

    if( definition != Definition.ALWAYS)
      {
      VarValueDefBuilder no = VarValueDefBuilder.with( "No");
      if( definition == Definition.REQUIRED)
        {
        no.type( VarValueDef.Type.FAILURE);
        }
      varDef.values( no.build());
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
    return instanceSchemaVars( api, instanceVarTag, instanceOptional, instanceSchema, null);
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> instanceSchemaVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema, Set<String> requiredTypes)
    {
    ComposedSchema composedSchema =
      instanceSchema instanceof ComposedSchema
      ? (ComposedSchema) instanceSchema
      : null;

    String instanceType =
      instanceSchema == null
      ? null
      : instanceSchema.getType();

    return
      // Missing schema (equivalent to an empty schema)?
      instanceSchema == null?
      Stream.of( instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema)) :

      // Unknown schema type?
      !isOpenApiType( instanceType)?
      unknownSchemaVars( instanceType) :

      // Composed schema?
      composedSchema != null?
      composedSchemaVars( api, instanceVarTag, instanceOptional, composedSchema, requiredTypes) :

      // Basic empty schema?
      instanceType == null?
      Stream.of( instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema)) :

      // Basic type schema
      typeSchemaVars( api, instanceType, instanceVarTag, instanceOptional, instanceSchema);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> typeSchemaVars( OpenAPI api, String instanceType, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    Stream<IVarDef> typeVars =
      "object".equals( instanceType)?
      instanceObjectVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "string".equals( instanceType)?
      instanceStringVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "integer".equals( instanceType)?
      instanceIntegerVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "boolean".equals( instanceType)?
      instanceBooleanVars( api, instanceVarTag, instanceOptional, instanceSchema) :

      "array".equals( instanceType)?
      instanceArrayVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      "number".equals( instanceType)?
      instanceNumberVars( api, instanceVarTag, instanceOptional, instanceSchema) :
      
      Stream.empty();
    
    return
      Stream.concat(
        typeVars,

        notVar( api, instanceVarTag, instanceSchema.getNot())
        .map( notVar -> Stream.of( notVar))
        .orElse( Stream.empty()));
    }

  /**
   * Returns an empty stream for a schema of unknown type.
   */
  private Stream<IVarDef> unknownSchemaVars( String type)
    {
    notifyError( String.format( "Unknown schema type=%s is not supported", type), "Ignoring unknown schema");
    return Stream.empty();
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given array instance.
   */
  private Stream<IVarDef> instanceArrayVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    Schema<?> itemSchema =
      instanceSchema instanceof ArraySchema
      ? resolveSchema( api, ((ArraySchema) instanceSchema).getItems())
      : null;
    
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),

        VarSetBuilder.with( "Items")
        .when( has( instanceValueProperty( instanceVarTag)))
        .members(
          arraySizeVar( api, instanceVarTag, instanceSchema))
        .members(
          iterableOf( arrayItemsVar( api, instanceVarTag, instanceSchema, itemSchema)))
        .members(
          iterableOf( arrayUniqueItemsVar( api, instanceVarTag, instanceSchema)))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the size of an array instance.
   */
  private IVarDef arraySizeVar( OpenAPI api, String instanceVarTag, Schema<?> arraySchema)
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
        .map( min -> Optional.ofNullable( maxItems).map( max -> adjustedMinOf( "Items", min, max)).orElse( min))
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
  private Optional<IVarDef> arrayItemsVar( OpenAPI api, String instanceVarTag, Schema<?> arraySchema, Schema<?> itemSchema)
    {
    return
      with( "items",
        () ->
        Optional.ofNullable( arraySchema.getMaxItems()).orElse( Integer.MAX_VALUE) <= 0

        ? Optional.empty()

        : Optional.of(
          VarSetBuilder.with( "Contains")
          .when( has( arrayItemsProperty( instanceVarTag)))
          .members( instanceSchemaVars( api, arrayItemsProperty( instanceVarTag), false, itemSchema))
          .build()));
    }

  /**
   * Returns the {@link IVarDef input variable} representing the unique items property of an array instance.
   */
  private Optional<IVarDef> arrayUniqueItemsVar( OpenAPI api, String instanceVarTag, Schema<?> arraySchema)
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
  private Stream<IVarDef> composedSchemaVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, ComposedSchema composedSchema, Set<String> requiredTypes)
    {
    // Verify composed schema types are consistent
    Set<String> composedTypes = getValidTypes( api, composedSchema);

    // Any composed schema type may be valid, unless specific parent schema types are required
    Set<String> validTypes = Optional.ofNullable( requiredTypes).orElse( composedTypes);

    Stream.Builder<IVarDef> composedSchemaVars = Stream.builder();
    if( !composedSchema.getAllOf().isEmpty())
      {
      composedSchemaVars.add( allOfVar( api, instanceVarTag, instanceOptional, validTypes, composedSchema.getAllOf()));
      }
    if( !composedSchema.getAnyOf().isEmpty())
      {
      composedSchemaVars.add( anyOfVar( api, instanceVarTag, instanceOptional, validTypes, composedSchema.getAnyOf()));
      }
    if( !composedSchema.getOneOf().isEmpty())
      {
      composedSchemaVars.add( oneOfVar( api, instanceVarTag, instanceOptional, validTypes, composedSchema.getOneOf()));
      }

    notVar( api, instanceVarTag, composedSchema.getNot())
      .ifPresent( notVar -> composedSchemaVars.add( notVar));

    return
      Stream.concat(
        composedSchema.getType() == null
        ? Stream.empty()
        : typeSchemaVars( api, composedSchema.getType(), instanceVarTag, instanceOptional, composedSchema),

        composedSchemaVars.build());
    }

  /**
   * Returns the {@link IVarDef input variable} defined by the given "allOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef allOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Set<String> validTypes, List<Schema> memberSchemas)
    {
    String containerType = "AllOf";
    String containerVarTag = instanceVarTag + containerType;
    List<Schema> applicableMembers = getApplicableMembers( api, "allOf", validTypes, memberSchemas);
    
    return
      VarSetBuilder.with( containerType)
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .members(
        VarSetBuilder.with( "Members")
        .members(
          IntStream.range( 0, applicableMembers.size())
          .mapToObj( i -> {
            return
              with( String.format( "allOf[%s]", i),
                () ->
                {
                String memberVarTag = containerVarTag + i;
                return
                  VarSetBuilder.with( String.valueOf(i))
                  .members( memberSchemaVars( api, containerVarTag, memberVarTag, applicableMembers.get(i), true, validTypes))
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
  private IVarDef anyOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Set<String> validTypes, List<Schema> memberSchemas)
    {
    return oneOfVar( api, instanceVarTag, instanceOptional, validTypes, "anyOf", memberSchemas);
    } 

  /**
   * Returns the {@link IVarDef input variable} defined by the given "oneOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef oneOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Set<String> validTypes, List<Schema> memberSchemas)
    {
    return oneOfVar( api, instanceVarTag, instanceOptional, validTypes, "oneOf", memberSchemas);
    } 

  /**
   * Returns the {@link IVarDef input variable} defined by the given "oneOf" or "anyOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef oneOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Set<String> validTypes, String containerType, List<Schema> memberSchemas)
    {
    String containerVarName = StringUtils.capitalize( containerType);
    String containerVarTag = instanceVarTag + containerVarName;
    List<Schema> applicableMembers = getApplicableMembers( api, containerType, validTypes, memberSchemas);
    boolean validationRequired = applicableMembers.size() == 1;
    boolean anyOf = containerType.equals( "anyOf");
    
    return
      VarSetBuilder.with( containerVarName)
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .members(
        VarSetBuilder.with( "Members")
        .members(
          iterableOf( oneOfValidatedVar( containerVarTag, validationRequired, anyOf)))
        .members(
          IntStream.range( 0, applicableMembers.size())
          .mapToObj( i -> {
            return
              with( String.format( "%s[%s]", containerType, i),
                () ->
                {
                String memberVarTag = containerVarTag + i;
                return
                  VarSetBuilder.with( String.valueOf(i))
                  .members( memberSchemaVars( api, containerVarTag, memberVarTag, applicableMembers.get(i), validationRequired, validTypes))
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
  private Stream<IVarDef> memberSchemaVars( OpenAPI api, String containerVarTag, String memberVarTag, Schema<?> memberSchema, boolean validationRequired, Set<String> validTypes)
    {
    List<IVarDef> memberSchemaVars = instanceSchemaVars( api, memberVarTag, false, memberSchema, validTypes).collect( toList());
    if( !validationRequired)
      {
      // To provide for a single validation failure, find all descendant variables that define a failure value...
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
   * Returns the {@link IVarDef input variable} defined by the "not" schema for the given instance.
   */
  private Optional<IVarDef> notVar( OpenAPI api, String instanceVarTag, Schema<?> notSchema)
    {
    if( notSchema != null)
      {
      notifyError( "The \"not\" keyword is not yet supported", "Ignoring the \"not\" schema");
      }

    return Optional.empty();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the type of a instance.
   */
  private IVarDef instanceTypeVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    String type = instanceSchema == null? null : instanceSchema.getType();
    Boolean nullable = instanceSchema == null? Boolean.FALSE : instanceSchema.getNullable();

    return
      VarDefBuilder.with( "Type")
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))

      .values(
        VarValueDefBuilder.with( type == null? "Not null" : type).properties( instanceValueProperty( instanceVarTag)).build(),
        VarValueDefBuilder.with( (Object) null).type( Boolean.TRUE.equals( nullable)? VarValueDef.Type.ONCE : VarValueDef.Type.FAILURE).build())

      .values(
        iterableOf(
          Optional.ofNullable(
            type == null
            ? null
            : VarValueDefBuilder.with( String.format( "Not %s", type)).type( VarValueDef.Type.FAILURE).build())))

      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an integer instance.
   */
  private IVarDef integerValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    VarDefBuilder value =
      VarDefBuilder.with( "Value")
      .has( "format", instanceSchema.getFormat())
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .when( has( instanceValueProperty( instanceVarTag)));

    // Enumerated values?
    List<Number> enums = getIntegerEnum( instanceSchema);
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
           
        // Ensure min/max range is feasible
        minimum = 
          Optional.ofNullable( minimum)
          .map( min -> Optional.ofNullable( maximum).map( max -> adjustedMinOf( "imum", min, max)).orElse( min))
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
    List<BigDecimal> enums = getNumberEnum( instanceSchema);
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
           
        // Ensure min/max range is feasible
        minimum = 
          Optional.ofNullable( minimum)
          .map( min -> Optional.ofNullable( maximum).map( max -> adjustedMinOf( "imum", min, max)).orElse( min))
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
  @SuppressWarnings("rawtypes")
  private IVarDef objectValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    // Ensure schema defined for all required properties, using a default empty schema if necessary.
    Map<String,Schema> propertyDefs = Optional.ofNullable( instanceSchema.getProperties()).orElse( new HashMap<String,Schema>());
    Optional.ofNullable( instanceSchema.getRequired())
      .map( required -> required.stream().filter( property -> !propertyDefs.containsKey( property)).collect( toList()))
      .filter( undefined -> !undefined.isEmpty())
      .ifPresent( undefined -> {
        undefined.stream().forEach( required -> propertyDefs.put( required, new Schema<Object>()));
        instanceSchema.setProperties( propertyDefs);
        });

    // Reconcile any "required" constraints for read/WriteOnly properties.
    instanceSchema.setRequired(
      Optional.ofNullable( instanceSchema.getRequired()).orElse( emptyList())
      .stream()
      .filter( property -> expectedInView( propertyDefs.get( property)))
      .collect( toList()));

    // Accumulate constraints on the number of properties expected.
    PropertyCountConstraints constraints = new PropertyCountConstraints();
    constraints.setRequiredCount( Optional.ofNullable( instanceSchema.getRequired()).orElse( emptyList()).size());
    constraints.setTotalCount( objectTotalProperties( instanceSchema));

    constraints.setHasAdditional(
      Optional.ofNullable( instanceSchema.getAdditionalProperties())
      .map( additional -> additional.getClass().equals( Boolean.class)? (Boolean) additional : true)
      .orElse( true));

    // Ensure min/max range is feasible
    instanceSchema.setMinProperties(
      Optional.ofNullable( instanceSchema.getMinProperties())
      .map( min -> Optional.ofNullable( instanceSchema.getMaxProperties()).map( max -> adjustedMinOf( "Properties", min, max)).orElse( min))
      .orElse( null));

    // Ensure minimum is a usable constraint
    instanceSchema.setMinProperties(
      Optional.ofNullable( instanceSchema.getMinProperties())
      .filter( min -> isUsablePropertyLimit( "minProperties", min, constraints))
      .orElse( null));

    // Ensure minimum is a usable constraint
    instanceSchema.setMaxProperties(
      Optional.ofNullable( instanceSchema.getMaxProperties())
      .filter( max -> isUsablePropertyMax( "maxProperties", max, constraints))
      .orElse( null));

    // Are additional properties are required to satisfy the minimum?
    Integer minProperties = instanceSchema.getMinProperties();
    constraints.setRequiresAdditional(
      Optional.ofNullable( minProperties)
      .map( min -> min > constraints.getTotalCount() && constraints.hasAdditional())
      .orElse( false));

    // Are all properties effectively required to satisfy the minimum?
    constraints.setAllRequired(
      Optional.ofNullable( minProperties)
      .map( min -> min == constraints.getTotalCount() && !constraints.hasAdditional())
      .orElse( false));

    return
      VarSetBuilder.with( "Value")
      .when( has( instanceValueProperty( instanceVarTag)))
      .members( iterableOf( objectPropertyCountVar( api, instanceVarTag, instanceSchema, constraints)))
      .members( objectPropertiesVar( api, instanceVarTag, instanceSchema, constraints))
      .build();
    }

  /**
   * Returns true if a property with this schema can be expected to appear in the API view produced by this InputModeller.
   */
  private boolean expectedInView( Schema<?> propertySchema)
    {
    return
      propertySchema == null?
      true :
      
      view_ == View.REQUEST?
      !Optional.ofNullable( propertySchema.getReadOnly()).orElse( false) :
      
      !Optional.ofNullable( propertySchema.getWriteOnly()).orElse( false);
    }

  /**
   * Returns true if a property with this schema must be excluded from the API view produced by this InputModeller.
   */
  private boolean excludedFromView( Schema<?> propertySchema)
    {
    return
      !expectedInView( propertySchema)
      &&
      (view_ == View.REQUEST
       ? options_.isReadOnlyEnforced()
       : options_.isWriteOnlyEnforced());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the number of properties for an object instance.
   */
  private Optional<IVarDef> objectPropertyCountVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema, PropertyCountConstraints constraints)
    {
    // Accumulate property count values depending on constraints
    ListBuilder<VarValueDef> countValues = ListBuilder.to();
    
    // Property count has usable constraints?
    Integer minProperties = instanceSchema.getMinProperties();
    Integer maxProperties = instanceSchema.getMaxProperties();
    if( !(minProperties == null && maxProperties == null))
      {
      // Yes, upper bound defined?
      if( maxProperties != null)
        {
        // Yes, with provision for any lower bound...
        if( minProperties != null)
          {
          belowMinPropertiesFailure( instanceVarTag, minProperties, constraints)
            .ifPresent( failure -> countValues.add( failure));
          }
        else if( constraints.getRequiredCount() == 0)
          {
          countValues.add(
            VarValueDefBuilder.with( 0)
            .when( not( objectPropertiesProperty( instanceVarTag))) 
            .type( VarValueDef.Type.ONCE)
            .build());
          }

        // ... and keeping in mind that maxProperties can exceed total count when additional properties are allowed...
        Optional<Integer> maxDefined =
          Optional.ofNullable(
            maxProperties > constraints.getTotalCount()
            ? constraints.getTotalCount() + 1
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
        ICondition minSatisfied =
          // All properties effectively required? If so, satisfaction of minProperties is the only successful outcome.
          constraints.allRequired()
          ? null
          : notLessThan( objectPropertiesProperty( instanceVarTag), Math.min( minProperties, constraints.getTotalCount()));
        
        countValues
          .add(
            VarValueDefBuilder.with( String.format( ">= %s", minProperties))
            .when( minSatisfied)
            .build());

        belowMinPropertiesFailure( instanceVarTag, minProperties, constraints)
          .ifPresent( failure -> countValues.add( failure));
        }
      }

    // No, but are all properties optional?
    else if( constraints.getRequiredCount() == 0 && constraints.getTotalCount() > 0)
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
   * Returns the total number of properties defined by the given object instance, less any that
   * must be excluded from the API view produced by this InputModeller.
   */
  private int objectTotalProperties( Schema<?> instanceSchema)
    {
    return
      (int) 
      Optional.ofNullable( instanceSchema.getProperties()).orElse( emptyMap())
      .entrySet().stream()
      .filter( e -> !excludedFromView( e.getValue()))
      .count();
    }

  /**
   * Return true if and only if the given limit on the number of object properties is usable.
   */
  private boolean isUsablePropertyLimit( String description, int limit, PropertyCountConstraints constraints)
    {
    boolean usable = false;

    if( !constraints.hasAdditional() && limit > constraints.getTotalCount())
      {
      notifyError(
        String.format( "%s=%s exceeds the total number of properties", description, limit),
        String.format( "Ignoring infeasible %s", description));
      }
    else if( limit < constraints.getRequiredCount())
      {
      notifyError(
        String.format( "%s=%s is less than required=%s", description, limit, constraints.getRequiredCount()),
        String.format( "Ignoring infeasible %s", description));
      }
    else if( limit == constraints.getRequiredCount())
      {
      notifyWarning(
        String.format( "%s=%s is superfluous -- same as required", description, limit));
      }
    else
      {
      usable = true;
      }
    
    return usable;
    }

  /**
   * Return true if and only if the given maximum on the number of object properties is usable.
   */
  private boolean isUsablePropertyMax( String description, int maximum, PropertyCountConstraints constraints)
    {
    boolean usable = false;

    if( !constraints.hasAdditional() && maximum == constraints.getTotalCount())
      {
      notifyWarning(
        String.format( "%s=%s is superfluous -- same as the total number of properties", description, maximum));
      }
    else 
      {
      usable = isUsablePropertyLimit( description, maximum, constraints);
      }
    
    return usable;
    }

  /**
   * Returns a property count value representing a failure to satisfy the required "minProperties" for the given object instance,
   * if an explicit check for this failure is needed. Otherwise, if an explicit check is superfluous, returns <CODE>Optional.empty()</CODE>.
   */
  private Optional<VarValueDef> belowMinPropertiesFailure( String instanceVarTag, int minProperties, PropertyCountConstraints constraints)
    {
    return
      constraints.requiresAdditional() || constraints.allRequired()?
      Optional.empty() :

      // Otherwise, verify failure when minimum is not satisfied
      Optional.of(
        VarValueDefBuilder.with( String.format( "< %s", minProperties))
        .when( lessThan( objectPropertiesProperty( instanceVarTag), minProperties)) 
        .type( VarValueDef.Type.FAILURE)
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the properties of an object instance.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef objectPropertiesVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema, PropertyCountConstraints constraints)
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
            constraints.allRequired() || requiredProperties.contains( propertyDef.getKey()),
            resolveSchema( api, propertyDef.getValue()))))

      .members(
        objectAdditionalVar( api, instanceVarTag, instanceSchema, constraints))

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
          excludedFromView( propertySchema)?

          VarSetBuilder.with( propertyVarName)
          .members( instanceDefinedVar( propertyVarTag, Definition.EXCLUDED))
          .build() :
          
          VarSetBuilder.with( propertyVarName)
          .members( instanceDefinedVar( propertyVarTag, required, objectPropertiesProperty( instanceVarTag)))
          .members( instanceSchemaVars( api, propertyVarTag, propertySchema))
          .build();
        });
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the additional properties of an object instance.
   */
  private IVarDef objectAdditionalVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema, PropertyCountConstraints constraints)
    {
    boolean allowed =
      constraints.hasAdditional()
      &&
      Optional.ofNullable( instanceSchema.getMaxProperties()).orElse( Integer.MAX_VALUE) > 0;

    boolean required =
      allowed
      &&
      Optional.ofNullable( instanceSchema.getMinProperties())
      .map( min -> min > constraints.getTotalCount())
      .orElse( false);

    Class<?> type =
      Optional.ofNullable( instanceSchema.getAdditionalProperties())
      .map( Object::getClass)
      .orElse( null);

    Schema<?> propertySchema =
      !Boolean.class.equals( type)
      ? (Schema<?>)instanceSchema.getAdditionalProperties()
      : null;
        
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
        // Yes, ensure min/max range is feasible
        minLength =
          Optional.ofNullable( minLength)
          .map( min -> Optional.ofNullable( maxLength).map( max -> adjustedMinOf( "Length", min, max)).orElse( min))
          .orElse( null);

        // Add boundary condition values
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
    List<Boolean> possibleValues = Arrays.asList( Boolean.TRUE, Boolean.FALSE);
    List<Boolean> allowedValues = Optional.of( getBooleanEnum( instanceSchema)).filter( enums -> !enums.isEmpty()).orElse( possibleValues);

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
   * Returns the adjusted minimum of the given range.
   */
  private <T extends Comparable<T>> T adjustedMinOf( String description, T min, T max)
    {
    if( min.compareTo( max) > 0)
      {
      notifyError(
        String.format(
          "min%s=%s is greater than max%s=%s",
          description, min,
          description, max),

        String.format(
          "Adjusting min%s to max%s",
          description,
          description));

      min = max;
      }

    return min;
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
   * Returns input variable tag for the given media type
   */
  private String mediaTypeVarTag( String contentVarTag, String mediaType)
    {
    return contentVarTag.replace( "Content", "") + mediaTypeVarName( mediaType);
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
   * Returns the given value if non-null. Otherwise, throws an exception.
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
    return
      schema == null
      ? null
      : resolveSchemaType( componentSchemaRef( api, schema.get$ref()).orElse( schema));
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

      .map( ref -> componentName( COMPONENTS_PARAMETERS_REF, ref))
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

      .map( ref -> componentName( COMPONENTS_SCHEMAS_REF, ref))
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

      .map( ref -> componentName( COMPONENTS_REQUEST_BODIES_REF, ref))
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

      .map( ref -> componentName( COMPONENTS_RESPONSES_REF, ref))
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

      .map( ref -> componentName( COMPONENTS_HEADERS_REF, ref))
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
   * Returns true if the given schema type is supported by OpenAPI.
   */
  private boolean isOpenApiType( String type)
    {
    return type == null || SCHEMA_TYPES.contains( type);
    }

  /**
   * If necessary, updates the type of the given schema based its properties.
   * If the type defined for this schema is valid and consistent, returns the updated schema.
   * Otherwise, throws an exception.
   */
  private Schema<?> resolveSchemaType( Schema<?> schema)
    {
    String declaredType = schema.getType();
    
    List<String> impliedTypes = 
    Stream.of(
      impliedType( "array", schema, Schema::getMaxItems, Schema::getMinItems, Schema::getUniqueItems),
      impliedType( "number", schema, Schema::getMaximum, Schema::getExclusiveMaximum, Schema::getMinimum, Schema::getExclusiveMinimum, Schema::getMultipleOf),
      impliedType( "object", schema, Schema::getMaxProperties, Schema::getMinProperties, Schema::getRequired, Schema::getProperties, Schema::getAdditionalProperties),
      impliedType( "string", schema, Schema::getMaxLength, Schema::getMinLength, Schema::getPattern))
      .filter( Objects::nonNull)
      .collect( toList());

    if( impliedTypes.size() == 1)
      {
      String impliedType = impliedTypes.get(0);
      if( declaredType == null)
        {
        schema.setType( impliedType);
        }
      else if( !(impliedType.equals( declaredType) || (impliedType.equals( "number") && declaredType.equals( "integer"))))
        {
        throw new IllegalStateException( String.format( "Schema declares type=%s but has implied type=%s", declaredType, impliedType));
        }
      }
    else if( impliedTypes.size() > 1)
      {
      throw new IllegalStateException( String.format( "Ambiguous schema type -- could be any of %s", impliedTypes.stream().collect( joining( ", "))));
      }
      
    return schema;
    }

  /**
   * Returns the instance types that can be validated by the given schema. Returns null if any type can be validated.
   */
  @SuppressWarnings("unchecked")
  private Set<String> getValidTypes( OpenAPI api, Schema<?> schema)
    {
    Map<String,Object> extensions = Optional.ofNullable( schema.getExtensions()).orElse( new HashMap<String,Object>());
    if( !extensions.containsKey( EXT_VALID_TYPES))
      {
      extensions.put( EXT_VALID_TYPES, findValidTypes( api, schema));
      }

    return (Set<String>) extensions.get( EXT_VALID_TYPES);
    }

  /**
   * Returns the instance types that can be validated by the given schema. Returns null if any type can be validated.
   */
  @SuppressWarnings("rawtypes")
  private Set<String> findValidTypes( OpenAPI api, Schema<?> schema)
    {
    // By default, only the type declared for this schema is valid.
    Set<String> validTypes =
      Optional.ofNullable( schema.getType())
      .map( type -> Stream.of( type).collect( toSet()))
      .orElse( null);

    ComposedSchema composedSchema = schema instanceof ComposedSchema? (ComposedSchema) schema : null;
    if( composedSchema != null)
      {
      // Resolve "allOf" schemas
      composedSchema.setAllOf(
        Optional.ofNullable( composedSchema.getAllOf()).orElse( emptyList())
        .stream()
        .map( member -> resolveSchema( api, member))
        .collect( toList()));
        
      // If "allOf" specified, valid types may include only those accepted by all members.
      List<Schema> allOfMembers = composedSchema.getAllOf();
      Set<String> allOfTypes =
        IntStream.range( 0, allOfMembers.size())
        .mapToObj( i -> new SimpleEntry<Integer,Set<String>>( i, getValidTypes( api, allOfMembers.get(i))))
        .filter( memberTypes -> memberTypes.getValue() != null)
        .reduce(
          (allTypes, memberTypes) ->
          with( String.format( "allOf[%s]", memberTypes.getKey()),
            () -> {
              allTypes.getValue().retainAll( memberTypes.getValue());
              if( allTypes.getValue().isEmpty())
                {
                throw
                  new IllegalStateException(
                    String.format( "Valid types=%s for this member are not accepted by other \"allOf\" members", memberTypes.getValue()));
                }
              return allTypes;
            }))
        .map( allTypes -> allTypes.getValue())
        .orElse( null);

      // Valid types include only those accepted by "allOf" and the rest of this schema.
      if( validTypes == null)
        {
        validTypes = allOfTypes;
        }
      else if( allOfTypes != null)
        {
        if( SetUtils.intersection( validTypes, allOfTypes).isEmpty())
          {
          throw new IllegalStateException( String.format( "\"allOf\" members accept types=%s but not required types=%s", allOfTypes, validTypes));
          }
        validTypes.retainAll( allOfTypes);
        }
      
      // Resolve "anyOf" schemas
      composedSchema.setAnyOf(
        Optional.ofNullable( composedSchema.getAnyOf()).orElse( emptyList())
        .stream()
        .map( member -> resolveSchema( api, member))
        .collect( toList()));
        
      // If "anyOf" specified, valid types may include any accepted by any member.
      Set<String> anyOfTypes =
        composedSchema.getAnyOf()
        .stream()
        .map( member -> getValidTypes( api, member))
        .filter( Objects::nonNull)
        .reduce((anyTypes, memberTypes) -> { anyTypes.addAll( memberTypes); return anyTypes;})
        .orElse( null);

      // Valid types include only those accepted by "anyOf" and the rest of this schema.
      if( validTypes == null)
        {
        validTypes = anyOfTypes;
        }
      else if( anyOfTypes != null)
        {
        if( SetUtils.intersection( validTypes, anyOfTypes).isEmpty())
          {
          throw new IllegalStateException( String.format( "\"anyOf\" members accept types=%s but not types=%s", anyOfTypes, validTypes));
          }
        validTypes.retainAll( anyOfTypes);
        }
      
      // Resolve "oneOf" schemas
      composedSchema.setOneOf(
        Optional.ofNullable( composedSchema.getOneOf()).orElse( emptyList())
        .stream()
        .map( member -> resolveSchema( api, member))
        .collect( toList()));
        
      // If "oneOf" specified, valid types may include any accepted by any member.
      Set<String> oneOfTypes =
        composedSchema.getOneOf()
        .stream()
        .map( member -> getValidTypes( api, member))
        .filter( Objects::nonNull)
        .reduce((oneTypes, memberTypes) -> { oneTypes.addAll( memberTypes); return oneTypes;})
        .orElse( null);

      // Valid types include only those accepted by "oneOf" and the rest of this schema.
      if( validTypes == null)
        {
        validTypes = oneOfTypes;
        }
      else if( oneOfTypes != null)
        {
        if( SetUtils.intersection( validTypes, oneOfTypes).isEmpty())
          {
          throw new IllegalStateException( String.format( "\"oneOf\" members accept types=%s but not types=%s", oneOfTypes, validTypes));
          }
        validTypes.retainAll( oneOfTypes);
        }
      }

    return validTypes;
    }

  /**
   * Returns the subset of the given member schemas that represent inputs that are applicable when only instance of the given
   * types are valid.
   */
  @SuppressWarnings("rawtypes")
  private List<Schema> getApplicableMembers( OpenAPI api, String containerType, Set<String> validTypes, List<Schema> memberSchemas)
    {
    return
      IntStream.range( 0, memberSchemas.size())
      .filter(
        i -> 
        with( String.format( "%s[%s]", containerType, i),
          () -> {
            boolean applicable = isApplicableInput( api, memberSchemas.get(i), validTypes);
            if( !applicable)
              {
              notifyWarning( String.format( "Ignoring this schema -- not applicable when only instance types=%s can be valid", validTypes));
              }
            return applicable;
          }))
      .mapToObj( i -> memberSchemas.get(i))
      .collect( toList());
    }

  /**
   * Returns true if and only if the given schema represents an input that is applicable when
   * only instances of the given types are valid.
   */
  private boolean isApplicableInput( OpenAPI api, Schema<?> schema, Set<String> validTypes)
    {
    Set<String> schemaTypes;
    return
      validTypes == null
      || (schemaTypes = getValidTypes( api, schema)) == null
      || !SetUtils.intersection( schemaTypes, validTypes).isEmpty();
    }

  /**
   * Returns the enumerated integer values defined by the given schema.
   */
  private List<Number> getIntegerEnum( Schema<?> schema)
    {
    List<Number> numbers;

    if( schema instanceof IntegerSchema)
      {
      numbers =
        Optional.ofNullable( ((IntegerSchema) schema).getEnum())
        .orElse( emptyList());
      }
    else
      {
      numbers = 
        Optional.ofNullable( schema.getEnum())
        .orElse( emptyList())
        .stream()
        .map(
          value -> {
            if( !(value == null || value.getClass().equals( Integer.class) || value.getClass().equals( Long.class)))
              {
              throw new IllegalStateException( String.format( "Enumerated value=%s is not an integer", value));
              }
            return (Number) value;
          })
        .collect( toList());
      }

    return numbers;
    }

  /**
   * Returns the enumerated number values defined by the given schema.
   */
  private List<BigDecimal> getNumberEnum( Schema<?> schema)
    {
    List<BigDecimal> numbers;

    if( schema instanceof NumberSchema)
      {
      numbers =
        Optional.ofNullable( ((NumberSchema) schema).getEnum())
        .orElse( emptyList());
      }
    else
      {
      numbers = 
        Optional.ofNullable( schema.getEnum())
        .orElse( emptyList())
        .stream()
        .map(
          value -> {
            if( !(value == null || Number.class.isAssignableFrom( value.getClass())))
              {
              throw new IllegalStateException( String.format( "Enumerated value=%s is not a number", value));
              }
            return value == null? null : new BigDecimal( value.toString());
          })
        .collect( toList());
      }

    return numbers;
    }

  /**
   * Returns the enumerated boolean values defined by the given schema.
   */
  private List<Boolean> getBooleanEnum( Schema<?> schema)
    {
    List<Boolean> booleans;

    if( schema instanceof BooleanSchema)
      {
      booleans =
        Optional.ofNullable( ((BooleanSchema) schema).getEnum())
        .orElse( emptyList());
      }
    else
      {
      booleans = 
        Optional.ofNullable( schema.getEnum())
        .orElse( emptyList())
        .stream()
        .map(
          value -> {
            Boolean booleanValue = null;
            if( value != null)
              {
              String valueString = value.toString();
              booleanValue =
                "true".equalsIgnoreCase( valueString)?
                Boolean.TRUE :

                "false".equalsIgnoreCase( valueString)?
                Boolean.FALSE :

                null;

              if( booleanValue == null)
                {
                throw new IllegalStateException( String.format( "Enumerated value=%s is not a boolean", value));
                }
              }
            
            return booleanValue;
          })
        .collect( toList());
      }

    return booleans;
    }

  /**
   * Returns the given type if applying any of the given accessors to the given schema produces a non-null value.
   * Otherwise, returns null.
   */
  @SafeVarargs
  private static String impliedType( String type, Schema<?> schema, Function<Schema<?>,Object>... accessors)
    {
    return
      Stream.of( accessors)
      .map( accessor -> accessor.apply( schema))
      .filter( Objects::nonNull)
      .findFirst()
      .map( value -> type)
      .orElse( null);
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
      throw new OpenApiException( contextLocation(), e);
      }
    finally
      {
      context_.removeLast();
      }
    }

  /**
   * Returns the path to the current context.
   */
  private String[] contextLocation()
    {
    return toStream( context_.iterator()).toArray( String[]::new);
    }

  /**
   * Report an warning condition
   */
  private void notifyWarning( String reason)
    {
    options_.getConditionNotifier().warn( contextLocation(), reason);
    }

  /**
   * Report an error condition
   */
  private void notifyError( String reason, String resolution)
    {
    options_.getConditionNotifier().error( contextLocation(), reason, resolution);
    }

  /**
   * Changes the options used by this InputModeller.
   */
  private void setOptions( ModelOptions options)
    {
    options_ = options;
    }

  /**
   * Returns the options used by this InputModeller.
   */
  public ModelOptions getOptions()
    {
    return options_;
    }

  /**
   * Defines the test requirement for the definition of a specified element.
   */
  private enum Definition
    {
      /**
       * This element must be defined. Otherwise, must report an error.
       */
      REQUIRED,

      /**
       * This element may or may not be defined.
       */
      OPTIONAL,

      /**
       * This element must NOT be defined. Otherwise, must report an error.
       */
      EXCLUDED,

      /**
       * This element will always be defined -- no verification needed.
       */
      ALWAYS,

      /**
       * This element will never be defined -- no verification needed.
       */
      NEVER
    };

  /**
   * Defines constraints on the number of properties expected by an object schema.
   */
  private static class PropertyCountConstraints
    {
    public void setRequiredCount( int requiredCount)
      {
      requiredCount_ = requiredCount;
      }

    public int getRequiredCount()
      {
      return requiredCount_;
      }
    
    public void setTotalCount( int totalCount)
      {
      totalCount_ = totalCount;
      }

    public int getTotalCount()
      {
      return totalCount_;
      }
    
    public void setHasAdditional( boolean hasAdditional)
      {
      hasAdditional_ = hasAdditional;
      }

    public boolean hasAdditional()
      {
      return hasAdditional_;
      }
    
    public void setRequiresAdditional( boolean requiresAdditional)
      {
      requiresAdditional_ = requiresAdditional;
      }

    public boolean requiresAdditional()
      {
      return requiresAdditional_;
      }
    
    public void setAllRequired( boolean allRequired)
      {
      allRequired_ = allRequired;
      }

    public boolean allRequired()
      {
      return allRequired_;
      }
    
    private int requiredCount_;
    private int totalCount_;
    private boolean hasAdditional_;
    private boolean requiresAdditional_;
    private boolean allRequired_;
    }

  private final View view_;
  private Deque<String> context_ = new ArrayDeque<String>();
  private ModelOptions options_;
  
  private static final String COMPONENTS_PARAMETERS_REF = "#/components/parameters/";
  private static final String COMPONENTS_REQUEST_BODIES_REF = "#/components/requestBodies/";
  private static final String COMPONENTS_RESPONSES_REF = "#/components/responses/";
  private static final String COMPONENTS_SCHEMAS_REF = "#/components/schemas/";
  private static final String COMPONENTS_HEADERS_REF = "#/components/headers/";
  private static final String EXT_VALID_TYPES = "x-tcases-valid-types";

  private static final Set<String> SCHEMA_TYPES =
    Arrays.asList( "array", "boolean", "integer", "number", "object", "string")
    .stream().collect( toSet());


}
