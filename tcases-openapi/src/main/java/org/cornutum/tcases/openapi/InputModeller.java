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
import static org.cornutum.tcases.openapi.OpenApiUtils.*;
import static org.cornutum.tcases.openapi.SchemaExtensions.*;
import static org.cornutum.tcases.openapi.SchemaUtils.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.servers.ServerVariable;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.math.RoundingMode.DOWN;
import static java.math.RoundingMode.UP;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.groupingBy;

/**
 * Converts between OpenAPI models and Tcases input models.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>.
 * See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public abstract class InputModeller extends ModelConditionReporter
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
    options_ = Optional.ofNullable( options).orElse( new ModelOptions());
    setContext( new NotificationContext( getOptions().getConditionNotifier()));
    analyzer_ = new SchemaAnalyzer( getContext());
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
      resultFor( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "version", info.getVersion())
          .hasIf( "server", membersOf( api.getServers()).findFirst().map( InputModeller::getServerUrl))
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
      resultFor( path,

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
      resultFor( opName,
        () ->
        op == null?
        null :

        FunctionInputDefBuilder.with( String.format( "%s_%s", opName, functionPathName( path)))
        .hasIf( "server", membersOf( pathItem.getServers()).findFirst().map( InputModeller::getServerUrl))
        .hasIf( "server", membersOf( op.getServers()).findFirst().map( InputModeller::getServerUrl))
        .has( "path", path)
        .has( "operation", opName)
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
      resultFor( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "version", info.getVersion())
          .hasIf( "server", membersOf( api.getServers()).findFirst().map( InputModeller::getServerUrl))
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
      resultFor( path,

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
      resultFor( opName,
        () ->
        op == null?
        null :

        FunctionInputDefBuilder.with( String.format( "%s_%s", opName, functionPathName( path)))
        .hasIf( "server", membersOf( pathItem.getServers()).findFirst().map( InputModeller::getServerUrl))
        .hasIf( "server", membersOf( op.getServers()).findFirst().map( InputModeller::getServerUrl))
        .vars( responsesVars( api, expectedValueOf( op.getResponses(), "responses")))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given request body.
   */
  private Optional<IVarDef> requestBodyVarDef( OpenAPI api, RequestBody body)
    {
    return
      resultFor( "requestBody",
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
      .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).has( "excluded", mediaTypes.keySet().stream()).build())
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
          resultFor( mediaType,
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
              contentVar.members( instanceSchemaVars( mediaTypeVarTag, false, analyzeSchema( api, mediaTypeSchema)));
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
      resultFor( parameter.getName(),
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
          .members( instanceSchemaVars( parameterVarName, parameterSchema))
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

    return analyzeSchema( api, schema);
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
      resultFor( "responses",
        () ->
        Stream.concat(
          Stream.of(
            VarDefBuilder.with( "Status-Code")
            .type( "response")
            .values( responseStatusValues( responses))
            .build()),

          responseVars( api, responses)));
    }

  /**
   * Returns the value definitions for the response status code variable.
   */
  private Stream<VarValueDef> responseStatusValues( ApiResponses responses)
    {
    List<String> statusCodes =
      responses.keySet().stream()
      .filter( status -> !status.equals( "default"))
      .collect( toList());
    
    return
      Stream.concat(
        // One for each specified status code...
        statusCodes.stream()
        .map( status -> VarValueDefBuilder.with( status).properties( statusCodeProperty( status)).build()),

        // And one for any unspecified status code...
        Stream.of( VarValueDefBuilder.with( "Other").has( "excluded", statusCodes.stream()).properties( statusCodeProperty( "Other")).build()));
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
          resultFor( status,
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
      resultFor( "headers",
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
      resultFor( headerName,
        () -> {
        String headerVarName = toIdentifier( headerName);
        String headerVarTag = status + "Header" + StringUtils.capitalize( headerVarName);
        Schema<?> headerSchema = analyzeSchema( api, header.getSchema());

        return
          VarSetBuilder.with( headerVarName)
          .members( headerDefinedVar( headerVarTag, header))
          .members( instanceSchemaVars( headerVarTag, headerSchema))
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
      resultFor( "content",
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
  private Stream<IVarDef> instanceSchemaVars( String instanceVarTag, Schema<?> instanceSchema)
    {
    return instanceSchemaVars( instanceVarTag, true, instanceSchema);      
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> instanceSchemaVars( String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    String instanceType = Optional.ofNullable( instanceSchema).map( Schema::getType).orElse( null);
    return
      // Missing schema (equivalent to an empty schema)?
      instanceSchema == null?
      Stream.of( instanceTypeVar( instanceVarTag, instanceOptional, instanceSchema)) :

      // Unknown schema type?
      !isSchemaType( instanceType)?
      unknownSchemaVars( instanceType) :

      // No, return all input variables for this schema
      allSchemaVars( instanceType, instanceVarTag, instanceOptional, instanceSchema);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by every alternative schema for the given instance.
   */
  private Stream<IVarDef> allSchemaVars( String instanceType, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    List<Schema<?>> alternatives =
      Optional.ofNullable( instanceSchema)
      .flatMap( s -> Optional.ofNullable( getDnf( s)))
      .map( dnf -> dnf.getAlternatives().stream().collect( toList()))
      .orElse( emptyList());

    return
      alternatives.isEmpty()?
      typeSchemaVars( instanceType, instanceVarTag, instanceOptional, instanceSchema) :
      
      alternatives.size() == 1?
      typeSchemaVars( instanceVarTag, instanceOptional, alternatives.get(0)) :

      alternativeSchemaVars( instanceVarTag, instanceOptional, alternatives);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by the given alternative schemas for the given instance.
   */
  private Stream<IVarDef> alternativeSchemaVars( String instanceVarTag, boolean instanceOptional, List<Schema<?>> alternatives)
    {
    return
      Stream.of(
        combinedAlternativesVar(
          VarSetBuilder.with( "Alternative")
          .members(
            VarDefBuilder.with( "Used")
            .values(
              IntStream.range( 0, alternatives.size())
              .mapToObj(
                i ->
                VarValueDefBuilder.with( String.valueOf( i))
                .properties( alternativeProperty( instanceVarTag, i))
                .build()))
            .build())

          .members(
            IntStream.range( 0, alternatives.size())
            .mapToObj(
              i->
              VarSetBuilder.with( String.valueOf( i))
              .when( has( alternativeProperty( instanceVarTag, i)))
              .members( typeSchemaVars( instanceVarTag, instanceOptional, alternatives.get(i)))
              .build()))
        
          .build()));
    }

  /**
   * Returns a simplified form of the given alternatives variable set, eliminating redundant and superfluous variable bindings.
   */
  private IVarDef combinedAlternativesVar( VarSet alternativesVar)
    {
    // For all alternative failure bindings...
    Map<String,List<VarBindingDef>> allFailures =
    toStream( new VarDefIterator( alternativesVar.getMembers()))
      .filter( var -> !"Used".equals( var.getName()))
      .flatMap( var -> toStream( var.getValues()).filter( value -> !value.isValid()).map( value -> new VarBindingDef( var, value)))
      .collect( groupingBy( binding -> alternativeVarName( binding.getVarDef())));

    // ...remove duplicate failure bindings...
    allFailures.keySet().stream()
      // ...for top-level alternatives only
      .filter( alternativeVar -> !alternativeVar.contains( ".Alternative."))
      .forEach( alternativeVar -> {
          allFailures.get( alternativeVar).stream()
          .collect( groupingBy( VarBindingDef::getValueDef))
          .forEach( (value,bindings) -> {
            Optional.of( bindings)
              .filter( b -> b.size() > 1)
              .map( b -> b.subList( 1, b.size()).stream())
              .ifPresent( dups -> dups.forEach( dup -> dup.getVarDef().removeValue( dup.getValueDef().getName())));
            });
        });
    
    return alternativesVar;
    }

  /**
   * Returns the path name of the given variable, relative to the parent of all alternatives
   */
  private String alternativeVarName( VarDef var)
    {
    return
      Optional.of( combinedAlternativeVarPattern_.matcher( var.getPathName()))
      .filter( Matcher::matches)
      .map( matcher -> matcher.group(1))
      .orElseThrow( () -> new IllegalStateException( String.format( "%s is not an alternative variable", var)));
    }

  /**
   * Returns the type-specific {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> typeSchemaVars( String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return typeSchemaVars( instanceSchema.getType(), instanceVarTag, instanceOptional, instanceSchema);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> typeSchemaVars( String instanceType, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    Stream.Builder<IVarDef> typeVars = Stream.builder();

    typeVars.add( instanceTypeVar( instanceVarTag, instanceOptional, instanceSchema));
    if( instanceType != null)
      {
      typeVars.add( typeValueVar( instanceType, instanceVarTag, instanceSchema));
      }

    return typeVars.build();
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variable} for the value defined by the given instance schema.
   */
  private IVarDef typeValueVar( String instanceType, String instanceVarTag, Schema<?> instanceSchema)
    {
    return
      "object".equals( instanceType)?
      objectValueVar( instanceVarTag, instanceSchema) :
      
      "string".equals( instanceType)?
      stringValueVar( instanceVarTag, instanceSchema) :
      
      "integer".equals( instanceType)?
      numberValueVar( instanceVarTag, instanceSchema) :
      
      "boolean".equals( instanceType)?
      booleanValueVar( instanceVarTag, instanceSchema) :

      "array".equals( instanceType)?
      arrayValueVar( instanceVarTag, instanceSchema) :
      
      // "number".equals( instanceType)
      numberValueVar( instanceVarTag, instanceSchema);
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
  private IVarDef arrayValueVar( String instanceVarTag, Schema<?> instanceSchema)
    {
    VarDef arraySizeVar = arraySizeVar( instanceVarTag, instanceSchema);

    VarSetBuilder valueVar =
      VarSetBuilder.with( "Items")
      .when( has( instanceValueProperty( instanceVarTag)))
      .members( arraySizeVar);
    
    Schema<?> itemSchema =
      Optional.ofNullable( asArraySchema( instanceSchema))
      .map( array -> array.getItems())
      .orElse( null);

    if( Optional.ofNullable( instanceSchema.getMaxItems()).orElse( Integer.MAX_VALUE) > 0)
      {
      valueVar
        .members( arrayItemsVar( instanceVarTag, itemSchema, arraySizeVar));
      }

    if( Optional.ofNullable( instanceSchema.getMaxItems()).orElse( Integer.MAX_VALUE) > 1)
      {
      valueVar
        .members( arrayUniqueItemsVar( instanceVarTag, instanceSchema));
      }
    
    return valueVar.build();
    }

  /**
   * Returns the {@link VarDef input variable} representing the size of an array instance.
   */
  private VarDef arraySizeVar( String instanceVarTag, Schema<?> arraySchema)
    {
    // Arrays size constrained?
    VarDefBuilder size = VarDefBuilder.with( "Size");
    Integer minItems = arraySchema.getMinItems();
    Integer maxItems = arraySchema.getMaxItems();
    if( minItems == null && maxItems == null)
      {
      // No, add standard boundary condition values
      size.values(
        VarValueDefBuilder.with( 0).properties( arrayItemsNoneProperty( instanceVarTag)).build(),
        VarValueDefBuilder.with( 1).build(),
        VarValueDefBuilder.with( "> 1").properties( arrayItemsManyProperty( instanceVarTag)).build());
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

      boolean allowSizeZero = (minItems == null || minItems == 0);
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
              .properties( Optional.of( arrayItemsNoneProperty( instanceVarTag)).filter( p -> sizeValue == 0 && allowSizeZero))
              .properties( Optional.of( arrayItemsManyProperty( instanceVarTag)).filter( p -> sizeValue > 1));
            }
          size.values( sizeBuilder.build());
          }
        }

      if( maxItems == null)
        {
        int many = Math.max( 1, minItems);
        size.values(
          VarValueDefBuilder.with( String.format( "> %s", many))
          .properties( arrayItemsManyProperty( instanceVarTag))
          .build());
        }
      else if( minItems == null)
        {
        size.values(
          VarValueDefBuilder.with( 0)
          .properties( arrayItemsNoneProperty( instanceVarTag))
          .build());
        if( maxItems > 1)
          {
          size.values(
            VarValueDefBuilder.with( String.format( "< %s", maxItems))
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
  private IVarDef arrayItemsVar( String instanceVarTag, Schema<?> itemSchema, VarDef arraySizeVar)
    {
    boolean allowSizeZero = Optional.ofNullable( arraySizeVar.getValue( 0)).map( VarValueDef::isValid).orElse( false);
    ICondition itemsCondition = allowSizeZero? not( arrayItemsNoneProperty( instanceVarTag)) : null;

    return
      resultFor( "items",
        () ->
        VarSetBuilder.with( "Contains")
        .when( itemsCondition)
        .members( instanceSchemaVars( arrayItemsProperty( instanceVarTag), false, itemSchema))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the unique items property of an array instance.
   */
  private IVarDef arrayUniqueItemsVar( String instanceVarTag, Schema<?> arraySchema)
    {
    boolean uniqueRequired = Boolean.TRUE.equals( arraySchema.getUniqueItems());

    return
      VarDefBuilder.with( "Unique")
      .when( has( arrayItemsManyProperty( instanceVarTag)))
      .values(
        VarValueDefBuilder.with( "Yes").build(),
        VarValueDefBuilder.with( "No").type( uniqueRequired? VarValueDef.Type.FAILURE: VarValueDef.Type.VALID).build())
      .build();
    }   

  /**
   * Returns the {@link IVarDef input variable} representing the type of a instance.
   */
  private IVarDef instanceTypeVar( String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    String type = instanceSchema == null? null : instanceSchema.getType();
    Boolean nullable = instanceSchema == null? Boolean.FALSE : instanceSchema.getNullable();
    Set<String> notTypes = Optional.ofNullable( instanceSchema).flatMap( s -> Optional.ofNullable( getNotTypes(s))).orElse( emptySet());

    return
      VarDefBuilder.with( "Type")
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))

      // Expectations for valid types
      .values(
        VarValueDefBuilder.with(
          Optional.ofNullable( type)
          .orElse( String.format( "Not %s", notTypes.isEmpty()? "null" : notTypes.stream().collect( joining( ",")))))
        .properties( instanceValueProperty( instanceVarTag)).build())

      // Expectations for null type
      .values(
        VarValueDefBuilder.with( (Object) null).type( Boolean.TRUE.equals( nullable)? VarValueDef.Type.ONCE : VarValueDef.Type.FAILURE).build())

      // Expectations for invalid types
      .values(
          Optional.ofNullable( type)
          .map( t -> Stream.of( String.format( "Not %s", t)))
          .orElse( notTypes.stream())
          .map( t -> VarValueDefBuilder.with( t).type( VarValueDef.Type.FAILURE).build()))

      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a number instance.
   */
  private IVarDef numberValueVar( String instanceVarTag, Schema<?> instanceSchema)
    {
    VarSetBuilder value =
      VarSetBuilder.with( "Value")
      .has( "format", instanceSchema.getFormat())
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .when( has( instanceValueProperty( instanceVarTag)));

    // Define values for the number quantity
    VarDefBuilder quantity = VarDefBuilder.with( "Is");

    BigDecimal multipleOf = instanceSchema.getMultipleOf();
    Set<BigDecimal> notMultipleOfs = Optional.ofNullable( getNotMultipleOfs( instanceSchema)).orElse( emptySet());
    boolean hasMultipleOfs = !(multipleOf == null && notMultipleOfs.isEmpty());
    Optional<String> unboundedProperty = Optional.ofNullable( hasMultipleOfs? unboundedValueProperty( instanceVarTag) : null);

    // Enumerated values?
    List<BigDecimal> enums = getNumberEnum( instanceSchema);
    List<BigDecimal> notEnums = getNumberEnum( getNotEnums( instanceSchema));
    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      quantity.values( enums.stream().map( i -> VarValueDefBuilder.with( i).build()));
      quantity.values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).has( "excluded", enums).build());
      }

    // Boundary conditions defined?
    else if( instanceSchema.getMinimum() == null && instanceSchema.getMaximum() == null)
      {
      // No, add standard boundary conditions
      TreeSet<BigDecimal> boundaryValues = new TreeSet<BigDecimal>();
      boundaryValues.add( BigDecimal.ZERO);
      boundaryValues.addAll( notEnums);

      quantity.values(
        VarValueDefBuilder.with( "< 0")
        .hasIf( "excluded", notEnums)
        .properties( unboundedProperty)
        .build());

      for( BigDecimal n : boundaryValues)
        {
        quantity.values
          ( VarValueDefBuilder.with( n)
            .type(
              notEnums.contains( n)
              ? VarValueDef.Type.FAILURE
              : VarValueDef.Type.VALID)
            .build());
        }
        
      quantity.values(
        VarValueDefBuilder.with( "> 0")
        .hasIf( "excluded", notEnums)
        .properties( unboundedProperty)
        .build());
      }
    else
      {
      // Yes, add min/max boundary condition values
      BigDecimal unit =
        new BigDecimal(
          BigInteger.ONE,
          Math.max(
            Optional.ofNullable( multipleOf).map( BigDecimal::scale).orElse( 0),
            Math.max(
              Optional.ofNullable( instanceSchema.getMinimum()).map( BigDecimal::scale).orElse( 0),
              Optional.ofNullable( instanceSchema.getMaximum()).map( BigDecimal::scale).orElse( 0))));

      BigDecimal effectiveMultipleOf =
        Optional.ofNullable( multipleOf)
        .orElse( unit);
    
      BigDecimal minimum =
        Optional.ofNullable( instanceSchema.getMinimum())
        .map( m -> multipleAbove( m, !Boolean.TRUE.equals( instanceSchema.getExclusiveMinimum()), effectiveMultipleOf, notMultipleOfs))
        .orElse( null);

      BigDecimal maximum =
        Optional.ofNullable( instanceSchema.getMaximum())
        .map( m -> multipleBelow( m, !Boolean.TRUE.equals( instanceSchema.getExclusiveMaximum()), effectiveMultipleOf, notMultipleOfs))
        .orElse( null);
           
      // Ensure min/max range is feasible
      BigDecimal effectiveMinimum = 
        Optional.ofNullable( minimum)
        .map( min -> Optional.ofNullable( maximum).map( max -> adjustedMinOf( "imum", min, max)).orElse( min))
        .orElse( null);

      TreeSet<BigDecimal> boundaryValues = new TreeSet<BigDecimal>();
      if( effectiveMinimum != null)
        {
        boundaryValues.add( effectiveMinimum);
        boundaryValues.add( multipleBelow( effectiveMinimum, false, effectiveMultipleOf, notMultipleOfs));
        }
      if( maximum != null)
        {
        boundaryValues.add( maximum);
        boundaryValues.add( multipleAbove( maximum, false, effectiveMultipleOf, notMultipleOfs));
        }
      boundaryValues.addAll( notEnums);

      for( BigDecimal n : boundaryValues)
        {
        quantity.values
          ( VarValueDefBuilder.with( n)
            .type(
              notEnums.contains( n) || (effectiveMinimum != null && n.compareTo(effectiveMinimum) < 0) || (maximum != null && n.compareTo(maximum) > 0)
              ? VarValueDef.Type.FAILURE
              : VarValueDef.Type.VALID)
            .build());
        }

      // For any missing boundary, add a value designated an unbounded range
      if( effectiveMinimum == null)
        {
        quantity.values(
          VarValueDefBuilder.with( String.format( "< %s", maximum))
          .properties( unboundedProperty)
          .build());
        }
      else if( maximum == null)
        {
        quantity.values(
          VarValueDefBuilder.with( String.format( "> %s", effectiveMinimum))
          .properties( unboundedProperty)
          .build());
        }
      else
        {
        // When fully bounded, select specific values to designate failures for any (not-)multiple-of constraints.
        if( multipleOf != null)
          {
          // Select a value within bounds that fails the multiple-of constraint.
          BigDecimal multipleOfFailure;
          for( multipleOfFailure = effectiveMinimum.add( unit);
               multipleOfFailure.compareTo( maximum) < 0 && isMultipleOf( multipleOfFailure, multipleOf);
               multipleOfFailure = multipleOfFailure.add( unit));
          if( multipleOfFailure.compareTo( maximum) < 0)
            {
            quantity.values(
              VarValueDefBuilder.with( multipleOfFailure)
              .type( VarValueDef.Type.FAILURE)
              .build());
            }
          }

        notMultipleOfs.stream()
          .forEach(
            m -> {
            // Select a value within bounds that fails each not-multiple-of constraint.
            BigDecimal notMultipleOfFailure;
            for( notMultipleOfFailure = effectiveMinimum.add( effectiveMultipleOf);
                 notMultipleOfFailure.compareTo( maximum) < 0 && !isMultipleOf( notMultipleOfFailure, m);
                 notMultipleOfFailure = notMultipleOfFailure.add( effectiveMultipleOf));
            if( notMultipleOfFailure.compareTo( maximum) < 0)
              {
              quantity.values(
                VarValueDefBuilder.with( notMultipleOfFailure)
                .type( VarValueDef.Type.FAILURE)
                .build());
              }
            });
        }
      }

    value.members( quantity.build());

    if(
      // Unbounded quantities allowed?
      enums.isEmpty() && (instanceSchema.getMinimum() == null || instanceSchema.getMaximum() == null)

      // (Not-)multiple-of constraints defined?
      && hasMultipleOfs)
      {
      // Yes, define values for (not-)multiple-of constraints on unbounded ranges
      VarSetBuilder multiples = VarSetBuilder.with( "Multiple-Of").when( has( unboundedValueProperty( instanceVarTag)));
      if( multipleOf != null)
        {
        multiples.members
          ( VarDefBuilder.with( toIdentifier( multipleOf))
            .values(
              VarValueDefBuilder.with( "Yes").has( "multipleOf", multipleOf).build(),
              VarValueDefBuilder.with( "No").type( VarValueDef.Type.FAILURE).build())
            .build());
        }
      for( BigDecimal m : notMultipleOfs)
        {
        multiples.members
          ( VarDefBuilder.with( toIdentifier( m))
            .values(
              VarValueDefBuilder.with( "Yes").type( VarValueDef.Type.FAILURE).has( "multipleOf", m).build(),
              VarValueDefBuilder.with( "No").build())
            .build());
        }

      value.members( multiples.build());
      }
    
    return value.build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an object instance.
   */
  @SuppressWarnings("rawtypes")
  private IVarDef objectValueVar( String instanceVarTag, Schema<?> instanceSchema)
    {
    // Ensure schema defined for all required properties, using a default empty schema if necessary.
    Map<String,Schema> propertyDefs = Optional.ofNullable( instanceSchema.getProperties()).orElse( new LinkedHashMap<String,Schema>());
    Optional.ofNullable( instanceSchema.getRequired())
      .map( required -> required.stream().filter( property -> !propertyDefs.containsKey( property)).collect( toList()))
      .filter( undefined -> !undefined.isEmpty())
      .ifPresent( undefined -> {
        undefined.stream().forEach( required -> propertyDefs.put( required, emptySchema()));
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
      // additionalProperties keyword is not false...
      Optional.ofNullable( instanceSchema.getAdditionalProperties())
      .map( additional -> additional.getClass().equals( Boolean.class)? (Boolean) additional : true)
      .orElse( true)
      &&
      // maxProperties not already satisfied by required properties
      Optional.ofNullable( instanceSchema.getMaxProperties())
      .map( max -> max > constraints.getRequiredCount())
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

    // Ensure maximum is a usable constraint
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
      .members( iterableOf( objectPropertyCountVar( instanceVarTag, instanceSchema, constraints)))
      .members( objectPropertiesVar( instanceVarTag, instanceSchema, constraints))
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
       ? getOptions().isReadOnlyEnforced()
       : getOptions().isWriteOnlyEnforced());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the number of properties for an object instance.
   */
  private Optional<IVarDef> objectPropertyCountVar( String instanceVarTag, Schema<?> instanceSchema, PropertyCountConstraints constraints)
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
  private IVarDef objectPropertiesVar( String instanceVarTag, Schema<?> instanceSchema, PropertyCountConstraints constraints)
    {
    Map<String,Schema> propertyDefs = Optional.ofNullable( instanceSchema.getProperties()).orElse( emptyMap());

    Set<String> notRequiredProperties = Optional.ofNullable( getNotRequired( instanceSchema)).orElse( emptySet());
    List<String> requiredProperties =
      Optional.ofNullable( instanceSchema.getRequired())
      .map( r -> r.stream().filter( p -> !notRequiredProperties.contains(p)).collect( toList()))
      .orElse( emptyList());

    return
      VarSetBuilder.with( "Properties")

      .members(
        propertyDefs.entrySet().stream()
        .map(
          propertyDef ->
          objectPropertyVar(
            instanceVarTag,
            propertyDef.getKey(),
            constraints.allRequired() || requiredProperties.contains( propertyDef.getKey()),
            propertyDef.getValue())))

      .members(
        objectAdditionalVar( instanceVarTag, instanceSchema, constraints))

      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an object instance property.
   */
  private IVarDef objectPropertyVar( String instanceVarTag, String propertyName, boolean required, Schema<?> propertySchema)
    {
    return
      resultFor( propertyName,
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
          .members( instanceSchemaVars( propertyVarTag, propertySchema))
          .build();
        });
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the additional properties of an object instance.
   */
  private IVarDef objectAdditionalVar( String instanceVarTag, Schema<?> instanceSchema, PropertyCountConstraints constraints)
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

    Schema<?> propertySchema =
      instanceSchema.getAdditionalProperties() instanceof Schema
      ? (Schema<?>)instanceSchema.getAdditionalProperties()
      : null;

    return
      propertySchema != null?
      objectPropertyVar( instanceVarTag, "Additional", required, propertySchema) :

      VarDefBuilder.with( "Additional")
      .values(
        VarValueDefBuilder.with( "Yes")
        .type( allowed? VarValueDef.Type.VALID : VarValueDef.Type.FAILURE)
        .properties( Optional.ofNullable( allowed? objectPropertiesProperty( instanceVarTag) : null))
        .build(),

        VarValueDefBuilder.with( "No")
        .type( required? VarValueDef.Type.FAILURE : VarValueDef.Type.VALID)
        .build())
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a string instance.
   */
  private IVarDef stringValueVar( String instanceVarTag, Schema<?> instanceSchema)
    {
    IVarDef valueVar;

    // Enumerated values?
    String format = instanceSchema.getFormat();
    List<FormattedString> enums = getFormattedStrings( "enumerated", format, instanceSchema.getEnum());
    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      valueVar = 
        VarDefBuilder.with( "Value")
        .has( "format", format)
        .has( "default", Objects.toString( getFormattedString( "default", format, instanceSchema.getDefault()), null))
        .when( has( instanceValueProperty( instanceVarTag)))
        .values( enums.stream().map( i -> VarValueDefBuilder.with( String.valueOf( i)).build()))
        .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).has( "excluded", enums).build())
        .build();
      }
    else
      {
      // No, add inputs for other string assertions.
      VarSetBuilder valueVarSet =
        VarSetBuilder.with( "Value")
        .has( "format", format)
        .has( "default", getFormattedString( "default", format, instanceSchema.getDefault()))
        .when( has( instanceValueProperty( instanceVarTag)));

      // Ensure min/max range is feasible
      Integer maxLength = instanceSchema.getMaxLength();
      Integer minLength = 
        Optional.ofNullable( instanceSchema.getMinLength())
        .map( min -> Optional.ofNullable( maxLength).map( max -> adjustedMinOf( "Length", min, max)).orElse( min))
        .orElse( null);

      // Get any excluded values (ignoring any with invalid length)
      List<FormattedString> notEnums =
        getFormattedStrings( "enumerated", format, getNotEnums( instanceSchema))
        .stream()
        .filter(
          excluded -> {
            int formattedLength = excluded.toString().length();
            return !(minLength != null && formattedLength < minLength) && !(maxLength != null && formattedLength > maxLength);
          })
        .collect( toList());
      Optional<String> notExcludedProperty = Optional.of( valueNotExcludedProperty( instanceVarTag)).filter( p -> !notEnums.isEmpty());
      Optional<ICondition> notExcluded = notExcludedProperty.map( p -> has( p));
      
      // Add values for any length assertions
      VarDefBuilder length = VarDefBuilder.with( "Length").when( notExcluded);

      // Any length assertions specified?
      if( minLength == null && maxLength == null)
        {
        // No, add standard values
        length.values( VarValueDefBuilder.with( "> 0").build());

        if(
          // Format allows empty string?
          (format == null || format.equals( "byte") || format.equals( "binary") || format.equals( "password"))
          &&
          // Empty string not excluded?
          !notEnums.stream().map( String::valueOf).anyMatch( String::isEmpty))
          {
          // Yes, allow empty values
          length.values( VarValueDefBuilder.with( 0).build());
          }
        }
      else
        {
        // Add boundary condition values
        TreeSet<Integer> boundaryValues = new TreeSet<Integer>();
        int effMinLength = Optional.ofNullable( minLength).orElse(0);
        boundaryValues.add( effMinLength);
        boundaryValues.add( effMinLength - 1);
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
                  (i < effMinLength) || (maxLength != null && i > maxLength)
                  ? VarValueDef.Type.FAILURE
                  : VarValueDef.Type.VALID)
                .build());
            }
          }
        if( minLength == null)
          {
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

      // Add variables for any pattern assertions
      String[] patterns = getPatterns( instanceSchema).stream().toArray(String[]::new);
      String[] notPatterns = Optional.ofNullable( getNotPatterns( instanceSchema)).orElse( emptySet()).stream().toArray(String[]::new);
      if( patterns.length == 1 && notPatterns.length == 0)
        {
        valueVarSet.members(
          VarDefBuilder.with( "Matches-Pattern")
          .when( notExcluded)
          .has( "pattern", patterns[0])
          .values(
            VarValueDefBuilder.with( "Yes").build(),
            VarValueDefBuilder.with( "No").type( VarValueDef.Type.FAILURE).build())
          .build());
        }
      else if( patterns.length == 0 && notPatterns.length == 1)
        {
        valueVarSet.members(
          VarDefBuilder.with( "Matches-Pattern")
          .when( notExcluded)
          .has( "pattern", notPatterns[0])
          .values(
            VarValueDefBuilder.with( "Yes").type( VarValueDef.Type.FAILURE).build(),
            VarValueDefBuilder.with( "No").build())
          .build());
        }
      else if( patterns.length > 0 || notPatterns.length > 0)
        {
        valueVarSet.members(
          VarSetBuilder.with( "Matches-Patterns")
          .when( notExcluded)
          .members(
            IntStream.range( 0, patterns.length)
            .mapToObj(
              i ->
              VarDefBuilder.with( String.valueOf(i))
              .has( "pattern", patterns[i])
              .values(
                VarValueDefBuilder.with( "Yes").build(),
                VarValueDefBuilder.with( "No").type( VarValueDef.Type.FAILURE).build())
              .build()))
          .members(
            IntStream.range( 0, notPatterns.length)
            .mapToObj(
              i ->
              VarDefBuilder.with( String.valueOf( patterns.length + i))
              .has( "pattern", notPatterns[i])
              .values(
                VarValueDefBuilder.with( "Yes").type( VarValueDef.Type.FAILURE).build(),
                VarValueDefBuilder.with( "No").build())
              .build()))
          .build());
        }

      // Add variables for any excluded values
      if( !notEnums.isEmpty())
        {
        valueVarSet.members(
          VarDefBuilder.with( "Is")
          .values(
            VarValueDefBuilder.with( "Any")
            .has( "excluded", notEnums)
            .properties( notExcludedProperty)
            .build())
          .values(
            notEnums.stream()
            .map(
              excluded ->
              VarValueDefBuilder.with( excluded)
              .type( VarValueDef.Type.FAILURE)
              .build()))
          .build());
        }

      // Complete inputs for all string assertions
      valueVar = valueVarSet.build();
      }

    return valueVar;
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a boolean instance.
   */
  private IVarDef booleanValueVar( String instanceVarTag, Schema<?> instanceSchema)
    {
    List<Boolean> possibleValues = Arrays.asList( Boolean.TRUE, Boolean.FALSE);
    List<Boolean> excludedValues = getBooleanEnum( getNotEnums( instanceSchema));

    List<Boolean> allowedValues =
      Optional.of( getBooleanEnum( instanceSchema))
      .filter( enums -> !enums.isEmpty())
      .orElse( possibleValues)
      .stream()
      .filter( b -> !excludedValues.contains( b))
      .collect( toList());

    if( allowedValues.isEmpty())
      {
      throw new IllegalStateException( "All possible boolean values have been excluded by this schema");
      }

    return
      VarDefBuilder.with( "Value")
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .when( has( instanceValueProperty( instanceVarTag)))
      .values(
        possibleValues.stream()
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
   * Returns true if the base value is a multiple of the additional value.
   */
  private boolean isMultipleOf( BigDecimal base, BigDecimal additional)
    {
    return
      base.compareTo( additional) >= 0
      &&
      base.remainder( additional).compareTo( BigDecimal.ZERO) == 0;
    }

  /**
   * Return the largest number less than or, if inclusive, equal to) the given value that satisfies the given (not-)multiple-of constraints.
   */
  private BigDecimal multipleBelow( BigDecimal value, boolean inclusive, BigDecimal multipleOf, Set<BigDecimal> notMultipleOfs)
    {
    BigDecimal below;

    for( below = inclusive? value : value.divide( multipleOf, 0, UP).subtract( BigDecimal.ONE).multiply( multipleOf);

         Stream.of( below)
           .filter( b -> notMultipleOfs.stream().anyMatch( m -> isMultipleOf( b, m)))
           .findAny()
           .isPresent();

         below = below.subtract( multipleOf));
           
    return below;
    }

  /**
   * Return the smallest number greater than (or, if inclusive, equal to) the given value that satisfies the given (not-)multiple-of constraints.
   */
  private BigDecimal multipleAbove( BigDecimal value, boolean inclusive, BigDecimal multipleOf, Set<BigDecimal> notMultipleOfs)
    {
    BigDecimal above;

    for( above = inclusive? value : value.divide( multipleOf, 0, DOWN).add( BigDecimal.ONE).multiply( multipleOf);

         Stream.of( above)
           .filter( a -> notMultipleOfs.stream().anyMatch( m -> isMultipleOf( a, m)))
           .findAny()
           .isPresent();

         above = above.add( multipleOf));
           
    return above;
    }

  /**
   * Returns the component of a function name that represents the given API request path.
   */
  static String functionPathName( String pathName)
    {
    return
      Arrays.stream( pathName.split( "/"))
      .flatMap( p -> pathTemplateIdentifiers( p))
      .filter( p -> !p.isEmpty())
      .collect( joining( "-"));
    }

  /**
   * Returns the sequence of identifiers defined by the given element of an API request path
   */
  static private Stream<String> pathTemplateIdentifiers( String pathElement)
    {
    Stream.Builder<String> identifiers = Stream.builder();
    Matcher segmentMatcher = uriSegmentPattern_.matcher( pathElement);
    while( segmentMatcher.find())
      {
      identifiers.add( toIdentifier( segmentMatcher.group().trim()));
      }
    return identifiers.build();
    }

  /**
   * Returns default URL for the given server.
   */
  static String getServerUrl( Server server)
    {
    StringBuilder url = new StringBuilder();
    Matcher segmentMatcher = uriSegmentPattern_.matcher( server.getUrl());
    while( segmentMatcher.find())
      {
      String constantSegment = segmentMatcher.group(1);
      url.append(
        constantSegment != null
        ? constantSegment
        : getServerVarValue( server, segmentMatcher.group(2)));
      }
    
    return url.toString();
    }

  /**
   * Returns the value of the given server variable
   */
  private static String getServerVarValue( Server server, String varName)
    {
    return
      Optional.ofNullable(
        Optional.ofNullable( server.getVariables())
        .map( vars -> vars.get( varName))
        .map( var -> Optional.ofNullable( var).map( ServerVariable::getDefault).orElse( null))
        .orElse( null))
      .orElseThrow( () -> new IllegalStateException( String.format( "No value defined for server variable=%s", varName)));
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
   * Returns the "unbounded value" property for the given instance.
   */
  private String unboundedValueProperty( String instanceTag)
    {
    return instanceTag + "Unbounded";
    }

  /**
   * Returns the "has items" property for the given array instance.
   */
  private String arrayItemsProperty( String instanceTag)
    {
    return instanceTag + "Items";
    }

  /**
   * Returns the "has no items" property for the given array instance.
   */
  private String arrayItemsNoneProperty( String instanceTag)
    {
    return arrayItemsProperty( instanceTag) + "None";
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
   * Returns the "not excluded value" property for the given schema instance.
   */
  private String valueNotExcludedProperty( String instanceTag)
    {
    return instanceTag + "NotExcluded";
    }

  /**
   * Returns the "has status code" property for the given status.
   */
  private String statusCodeProperty( String status)
    {
    return "status" + StringUtils.capitalize( status);
    }

  /**
   * Returns the "i'th alternative" property for the given schema instance.
   */
  private String alternativeProperty( String instanceTag, int i)
    {
    return instanceTag + "Alternative" + i;
    }

  /**
   * Returns the enumerated number values defined by the given schema.
   */
  private List<BigDecimal> getNumberEnum( Schema<?> schema)
    {
    return
      schema instanceof NumberSchema
      ? Optional.ofNullable( ((NumberSchema) schema).getEnum()).orElse( emptyList())
      : getNumberEnum( schema.getEnum());
    }

  /**
   * Returns the enumerated number values defined by the given list.
   */
  private List<BigDecimal> getNumberEnum( List<?> values)
    {
    return
      Optional.ofNullable( values)
      .orElse( emptyList())
      .stream()
      .map( value -> {
        try
          {
          return value == null? null : new BigDecimal( value.toString());
          }
        catch( Exception e)
          {
          throw new IllegalStateException( String.format( "Enumerated value=%s is not a number", value));
          }
        })
      .collect( toList());
    }

  /**
   * Returns the enumerated boolean values defined by the given schema.
   */
  private List<Boolean> getBooleanEnum( Schema<?> schema)
    {
    return
      schema instanceof BooleanSchema
      ? Optional.ofNullable( ((BooleanSchema) schema).getEnum()).orElse( emptyList())
      : getBooleanEnum( schema.getEnum());
    }

  /**
   * Returns the enumerated boolean values defined by the given list.
   */
  private List<Boolean> getBooleanEnum( List<?> values)
    {
    return
      Optional.ofNullable( values)
      .orElse( emptyList())
      .stream()
      .map( value -> {
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

  /**
   * Returns the given value as a formatted string.
   */
  private FormattedString getFormattedString( String description, String format, Object value)
    {
    try
      {
      return FormattedString.of( format, value);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Invalid %s value", description), e);
      }
    }

  /**
   * Returns the given values as a formatted strings.
   */
  private List<FormattedString> getFormattedStrings( String description, String format, List<?> value)
    {
    try
      {
      return FormattedString.of( format, value);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Invalid %s value", description), e);
      }
    }

  /**
   * Returns a fully-analyzed version of the given schema.
   */
  private Schema<?> analyzeSchema( OpenAPI api, Schema<?> schema)
    {
    return analyzer_.analyze( api, schema);
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
  private final ModelOptions options_;
  private final SchemaAnalyzer analyzer_;

  private static final Pattern uriSegmentPattern_ = Pattern.compile( "([^{}]+)|\\{([^}]+)\\}");
  private static final Pattern combinedAlternativeVarPattern_ = Pattern.compile( "(?:.*\\.)?Alternative\\.\\d+\\.(.*)");
}
