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

import org.cornutum.regexpgen.RegExpGen;
import static org.cornutum.regexpgen.Bounds.bounded;

import com.fasterxml.jackson.databind.node.ArrayNode;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
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
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

/**
 * Converts between OpenAPI models and Tcases input models.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>.
 * See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public abstract class InputModeller extends ConditionReporter<OpenApiContext>
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
    super( new OpenApiContext());
    view_ = expectedValueOf( view, "Model view");

    options_ = Optional.ofNullable( options).orElse( new ModelOptions());
    setNotifier( getOptions().getConditionNotifier());

    analyzer_ = new SchemaAnalyzer( getContext());
    analyzer_.setNotifier( getNotifier());
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  protected SystemInputDef requestInputModel( OpenAPI api)
    {
    return
      getOptions().getSource() == ModelOptions.Source.EXAMPLES
      ? requestExamplesModel( api)
      : requestSchemasModel( api);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by schemas in the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  private SystemInputDef requestSchemasModel( OpenAPI api)
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
      throw new OpenApiException( "Invalid API definition", e);
      }

    return
      resultFor( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "title", title)
          .has( "version", info.getVersion())
          .hasIf( "server", serverUriUsed( api.getServers()))
          .functions( entriesOf( api.getPaths()).flatMap( path -> pathRequestDefs( api, path.getKey(), path.getValue())))
          .build();

        return
          inputDef.getFunctionInputDefs().hasNext()
          ? inputDef
          : null;
        });
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by examples in the given
   * OpenAPI definition. Returns null if the given definition defines no API request examples to model.
   */
  private SystemInputDef requestExamplesModel( OpenAPI api)
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
      throw new OpenApiException( "Invalid API definition", e);
      }

    return
      resultFor( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "title", title)
          .has( "version", info.getVersion())
          .hasIf( "server", serverUriUsed( api.getServers()))
          .functions( entriesOf( api.getPaths()).flatMap( path -> pathRequestExamples( api, path.getKey(), path.getValue())))
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
      .filter( Objects::nonNull));
    }

  /**
   * Returns a request {@link FunctionInputDef function input definition} for the examples of each of the API operations for the given path.
   */
  private Stream<FunctionInputDef> pathRequestExamples( OpenAPI api, String path, PathItem pathItem)
    {
    return
      resultFor( path,

      () ->
      Stream.of(
        opRequestExamples( api, path, pathItem, "GET", pathItem.getGet()),
        opRequestExamples( api, path, pathItem, "PUT", pathItem.getPut()),
        opRequestExamples( api, path, pathItem, "POST", pathItem.getPost()),
        opRequestExamples( api, path, pathItem, "DELETE", pathItem.getDelete()),
        opRequestExamples( api, path, pathItem, "OPTIONS", pathItem.getOptions()),
        opRequestExamples( api, path, pathItem, "HEAD", pathItem.getHead()),
        opRequestExamples( api, path, pathItem, "PATCH", pathItem.getPatch()),
        opRequestExamples( api, path, pathItem, "TRACE", pathItem.getTrace()))

      // Skip if operation not defined
      .filter( Objects::nonNull));
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
        .hasIf( "server", serverUriUsed( pathItem.getServers()))
        .hasIf( "server", serverUriUsed( op.getServers()))
        .has( "path", path)
        .has( "operation", opName)
        .vars( opRequestVars( api, pathItem, op))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the operation request.
   */
  private Stream<IVarDef> opRequestVars( OpenAPI api, PathItem pathItem, Operation op)
    {
    return
      hasInputs( api, pathItem, op)?

      Stream.concat(
        opParameters( pathItem, op).map( p -> parameterVarDef( api, resolveParameter( api, p))),
        Stream.concat(
          requestBodyVarDef( api, op.getRequestBody()).map( Stream::of).orElse( Stream.empty()),
          authVarDef( api, op.getSecurity()).map( Stream::of).orElse( Stream.empty()))) :

      Stream.of( noInputs());
    }

  /**
   * Returns the request {@link FunctionInputDef function input definition} for the examples of the given API operation.
   */
  private FunctionInputDef opRequestExamples( OpenAPI api, String path, PathItem pathItem, String opName, Operation op)
    {
    return
      resultFor( opName, () -> {
        return
          op == null?
          null :

          withoutFailures(         
            FunctionInputDefBuilder.with( String.format( "%s_%s", opName, functionPathName( path)))
            .hasIf( "server", serverUriUsed( pathItem.getServers()))
            .hasIf( "server", serverUriUsed( op.getServers()))
            .has( "path", path)
            .has( "operation", opName)
            .vars( opExampleVars( api, pathItem, op))
            .build());
        });
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the examples of the given operation request.
   */
  private Stream<IVarDef> opExampleVars( OpenAPI api, PathItem pathItem, Operation op)
    {
    return
      hasInputs( api, pathItem, op)?

      Stream.concat(
        opParameters( pathItem, op).map( p -> parameterExamples( api, resolveParameter( api, p))),
        Stream.concat(
          requestBodyExamples( api, op.getRequestBody()).map( Stream::of).orElse( Stream.empty()),
          authVarDef( api, op.getSecurity()).map( Stream::of).orElse( Stream.empty()))) :

      Stream.of( noInputs());
    }

  /**
   * Returns true if at least one input is defined for the given request.
   */
  private boolean hasInputs( OpenAPI api, PathItem pathItem, Operation op)
    {
    return
      opParameters( pathItem, op).findAny().isPresent()
      ||
      op.getRequestBody() != null
      ||
      Optional.ofNullable( op.getSecurity())
      .map( secReqs -> !secReqs.isEmpty())
      .orElse( Optional.ofNullable( api.getSecurity()).filter( secReqs -> !secReqs.isEmpty()).isPresent())
      ;
    }

  /**
   * Returns a dummy variable representing a "no inputs" condition for an API request.
   */
  private IVarDef noInputs()
    {
    String none = "None";
    return
      VarSetBuilder.with( none)
      .type( "implicit")
      .members( instanceDefinedVar( none, Definition.NEVER))
      .build();
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
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
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
      throw new OpenApiException( "Invalid API definition", e);
      }

    return
      resultFor( title,
        () -> {
        SystemInputDef inputDef =
          SystemInputDefBuilder.with( toIdentifier( title))
          .has( "title", title)
          .has( "version", info.getVersion())
          .hasIf( "server", serverUriUsed( api.getServers()))
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
      .filter( Objects::nonNull));
    }

  /**
   * Returns the given {@link FunctionInputDef function input definition} after removing failure values for all variables.
   */
  private FunctionInputDef withoutFailures( FunctionInputDef functionDef)
    {
    toStream( new VarDefIterator( functionDef))
      .forEach( varDef -> {
        toStream( varDef.getFailureValues())
          .map( VarValueDef::getName)
          .collect( toList())
          .stream()
          .forEach( value -> varDef.removeValue( value));
        });
    
    return functionDef;
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
        .hasIf( "server", serverUriUsed( pathItem.getServers()))
        .hasIf( "server", serverUriUsed( op.getServers()))
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
          Map<String,MediaType> mediaTypes = expectedValueOf( ifNotEmpty( b.getContent()).orElse( null), "Request body content");
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
   * Returns the {@link IVarDef input variable definition} for the given request body examples.
   */
  private Optional<IVarDef> requestBodyExamples( OpenAPI api, RequestBody body)
    {
    return
      resultFor( "requestBody",
        () ->
        Optional.ofNullable( body)
        .map( b -> resolveRequestBody( api, b))
        .map( b -> {
          String contentVarTag = "Content";
          Map<String,MediaType> mediaTypes = expectedValueOf( ifNotEmpty( b.getContent()).orElse( null), "Request body content");
          return
            VarSetBuilder.with( "Body")
            .type( "request")
            .members(
              instanceDefinedVar( contentVarTag, Boolean.TRUE.equals( b.getRequired())),
              mediaTypeVar( contentVarTag, mediaTypes))
            .members(
              mediaTypeContentExamples( api, contentVarTag, mediaTypes))
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

            Schema<?> mediaTypeSchema =
              Optional.ofNullable( contentDef.getValue().getSchema())
              .orElseGet( () -> { notifyWarning( "No schema defined"); return emptySchema();});
            contentVar.members( instanceSchemaVars( mediaTypeVarTag, false, analyzeSchema( api, mediaTypeSchema)));

            return contentVar.build();
            });
        });
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given media type content examples.
   */
  private Stream<IVarDef> mediaTypeContentExamples( OpenAPI api, String contentVarTag, Map<String,MediaType> mediaTypes)
    {
    return
      mediaTypes.entrySet().stream()
      .map(
        contentDef ->
        {
        String mediaTypeName = contentDef.getKey();

        return
          resultFor( mediaTypeName,
            () -> {
            String mediaTypeVarName = mediaTypeVarName( mediaTypeName);
            String mediaTypeVarTag = mediaTypeVarTag( contentVarTag, mediaTypeName);
            MediaType mediaType = contentDef.getValue();

            Schema<?> mediaTypeSchema =
              Optional.ofNullable( mediaType.getSchema())
              .map( s -> exampleSchemaFor( mediaType.getExample(), mediaType.getExamples(), analyzeSchema( api, s)))
              .orElse( null);

            return
              VarSetBuilder.with( mediaTypeVarName)
              .when( has( mediaTypeVarTag))
              .members( instanceSchemaVars( mediaTypeVarTag, false, mediaTypeSchema))
              .build();
            });
        });
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given security requirements.
   */
  private Optional<IVarDef> authVarDef( OpenAPI api, List<SecurityRequirement> opSecurity)
    {
    return
      resultFor( "security", () -> {
        List<SecurityRequirement> security =
          secReqsSupported(
            api,
            
            Optional.ofNullable( opSecurity)
            .orElse(
              Optional.ofNullable( api.getSecurity())
              .orElse( emptyList())));
        
        return
          security.isEmpty()?
          Optional.empty() :

          Optional.of(
            VarSetBuilder.with( "Auth")
            .type( "security")
            .members(
              security.size() == 1
              ? authReqVars( api, security.get(0))
              : authReqVars( api, security))
            .build());
        });
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given security requirement.
   */
  private Stream<IVarDef> authReqVars( OpenAPI api, SecurityRequirement secReq)
    {
    return authReqVars( api, null, secReq);
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given security requirement.
   */
  private Stream<IVarDef> authReqVars( OpenAPI api, String secReqTag, SecurityRequirement secReq)
    {
    boolean schemesMany = secReq.size() > 1;
    boolean seqReqMany = secReqTag != null;
    boolean hasDefined = schemesMany || !seqReqMany;
    
    return
      secReq.keySet().stream()
      .map( name -> {
        return
          resultFor(
            name,
            () -> {

            Optional<IVarDef> defined =
              hasDefined?

              Optional.of(
                VarDefBuilder.with( "Defined")
                .values(
                  VarValueDefBuilder.with( "Yes")
                  .properties( secSchemeProperty( secReqTag, name))
                  .build(),
                  VarValueDefBuilder.with( "No")
                  .type( VarValueDef.Type.FAILURE)
                  .has( "authFailure", true)
                  .build())
                .build()) :

              Optional.empty();

            Optional<ICondition> isDefined =
              hasDefined
              ? Optional.of( has( secSchemeProperty( secReqTag, name)))
              : Optional.empty();
            
            return
              VarSetBuilder.with( name)
              .when( Optional.ofNullable( secReqTag).map( srt -> has( secReqProperty( secReqTag))))
              .members( streamOf( defined))
              .members( authSchemeVars( isDefined, getSecurityScheme( api, name)))
              .build();
            });
        });
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given security scheme.
   */
  private Stream<IVarDef> authSchemeVars( Optional<ICondition> isDefined, SecurityScheme secScheme)
    {
    Stream.Builder<IVarDef> vars = Stream.builder();

    switch( secScheme.getType())
      {
      case APIKEY:
        {
        vars.add(
          VarDefBuilder.with( "Type")
          .when( isDefined)
          .values( VarValueDefBuilder.with( secScheme.getType()).build())
          .build());

        vars.add(
          VarDefBuilder.with( "Location")
          .when( isDefined)
          .values( VarValueDefBuilder.with( secScheme.getIn()).build())
          .build());

        vars.add(
          VarDefBuilder.with( "Name")
          .when( isDefined)
          .values( VarValueDefBuilder.with( secScheme.getName()).build())
          .build());
        
        break;
        }
        
      case HTTP:
        {
        vars.add(
          VarDefBuilder.with( "Type")
          .when( isDefined)
          .values( VarValueDefBuilder.with( secScheme.getType()).build())
          .build());

        vars.add(
          VarDefBuilder.with( "Scheme")
          .when( isDefined)
          .values( VarValueDefBuilder.with( secScheme.getScheme().toLowerCase()).build())
          .build());
            
        break;
        }
        
      default:
        {
        throw new IllegalStateException( String.format( "Security type=%s is not supported in this version", secScheme.getType()));
        }
      }
    
    return vars.build();
    }

  /**
   * Returns the {@link IVarDef input variable definitions} for the given security requirements.
   */
  private Stream<IVarDef> authReqVars( OpenAPI api, List<SecurityRequirement> security)
    {
    IVarDef satisfied =
      VarDefBuilder.with( "Satisfied")
      .values(
        VarValueDefBuilder.with( "Yes")
        .when( equalTo( secReqProperty(), 1))
        .build(),

        VarValueDefBuilder.with( "No")
        .type( VarValueDef.Type.FAILURE)
        .has( "authFailure", true)
        .when( equalTo( secReqProperty(), 0))
        .build())
      .build();
      
    return
      Stream.concat(
        Stream.of( satisfied),

        IntStream.range( 0, security.size())
        .mapToObj( i -> {

          String secReqTag = String.valueOf( i);

          return
            resultFor(
              secReqTag,
              () -> {

              IVarDef defined = 
                VarDefBuilder.with( "Defined")
                .values(
                  VarValueDefBuilder.with( "Yes")
                  .properties( secReqProperty(), secReqProperty( secReqTag))
                  .build(),

                  VarValueDefBuilder.with( "No")
                  .build())
                .build();
              
              return
                VarSetBuilder.with( secReqTag)
                .members(
                  Stream.concat(
                    Stream.of( defined),
                    authReqVars( api, secReqTag, security.get(i))))
                .build();
              });                       
          }));
    }

  /**
   * Returns the members of the given set of security requirements, excluding any unsupported security schemes.
   */
  private List<SecurityRequirement> secReqsSupported( OpenAPI api, List<SecurityRequirement> secReqs)
    {
    return
      secReqs.stream()
      
      .map( secRec -> {
        List<String> schemesSupported =
          secRec.keySet().stream()
          .filter( name -> {
            return
              resultFor(
                name,
                () -> {
                  SecurityScheme scheme = getSecurityScheme( api, name);
                  boolean supported;
                  switch( scheme.getType())
                    {
                    case APIKEY:
                      {
                      supported = true;
                      break;
                      }
        
                    case HTTP:
                      {
                      supported = "basic".equalsIgnoreCase( scheme.getScheme()) || "bearer".equalsIgnoreCase( scheme.getScheme());
                      if( !supported)
                        {
                        notifyError(
                          String.format( "HTTP authentication scheme=%s is not supported in this version", scheme.getScheme()),
                          "Generated tests will not provide this type of authentication");
                        }
                      break;
                      }
        
                    default:
                      {
                      supported = false;
                      notifyError(
                        String.format( "Security type=%s is not supported in this version", scheme.getType()),
                        "Generated tests will not provide this type of authentication");
                      break;
                      }
                    }

                  return supported;
                });
            })
          .collect( toList());

        SecurityRequirement supported = new SecurityRequirement();
        for( String scheme : schemesSupported)
          {
          supported.addList( scheme, secRec.get( scheme));
          }
        return supported;
        })
      
      .filter( secReq -> !secReq.isEmpty())
      .collect( toList());
    }
  
  /**
   * Returns the {@link IVarDef input variable definition} for the given parameter.
   */
  private IVarDef parameterVarDef( OpenAPI api, Parameter parameter)
    {
    String parameterName = parameter.getName();
    return
      resultFor( parameterName,
        () ->
        {
        String parameterIn = expectedValueOf( parameter.getIn(), "in", parameterName);
        if( parameterIn.equals( "cookie") && !Characters.TOKEN.allowed( parameterName))
          {
          throw new IllegalStateException( String.format( "Parameter name='%s' contains characters not allowed in a cookie name", parameterName));
          }

        // Normalize parameter properties
        Schema<?> parameterSchema = parameterSchema( api, parameter);
        String parameterType = parameterSchema.getType();
        parameter.setStyle( parameterStyle( parameter, parameterType));
        parameter.setExplode( parameterExplode( parameter.getExplode(), getValidTypes( parameterSchema), parameter.getStyle()));

        // Normalize parameter schema
        normalizeParameterDnf( parameter, parameterSchema);
        
        String parameterVarName = toIdentifier( parameterName);
        return
          VarSetBuilder.with( parameterVarName)
          .type( parameter.getIn())
          .has( "paramName", parameterName)
          .members( parameterDefinedVar( parameterVarName, parameter))
          .members( instanceSchemaVars( parameterVarName, parameterSchema))
          .build();
        });
    }    

  /**
   * Returns the {@link IVarDef input variable definition} for the given parameter examples.
   */
  private IVarDef parameterExamples( OpenAPI api, Parameter parameter)
    {
    String parameterName = parameter.getName();
    return
      resultFor( parameterName,
        () ->
        {
        String parameterIn = expectedValueOf( parameter.getIn(), "in", parameterName);
        if( parameterIn.equals( "cookie") && !Characters.TOKEN.allowed( parameterName))
          {
          throw new IllegalStateException( String.format( "Parameter name='%s' contains characters not allowed in a cookie name", parameterName));
          }

        // Normalize parameter properties
        Schema<?> parameterSchema =
          analyzeSchema(
            api,
            exampleSchemaFor(
              parameterContentExample( parameter),
              parameterContentExamples( parameter),
              parameterSchema( api, parameter)));
        
        String parameterType = parameterSchema.getType();
        parameter.setStyle( parameterStyle( parameter, parameterType));
        parameter.setExplode( parameterExplode( parameter.getExplode(), getValidTypes( parameterSchema), parameter.getStyle()));

        // Normalize parameter schema
        normalizeParameterDnf( parameter, parameterSchema);
        
        String parameterVarName = toIdentifier( parameterName);
        return
          VarSetBuilder.with( parameterVarName)
          .type( parameterIn)
          .has( "paramName", parameterName)
          .members( parameterDefinedVar( parameterVarName, parameter))
          .members( instanceSchemaVars( parameterVarName, !parameter.getRequired(), parameterSchema))
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
      .orElseGet(
        () ->
        parameterMediaType( parameter)
        .map( mediaType -> Optional.ofNullable( mediaType.getSchema()).orElseGet( () -> { notifyWarning( "No schema defined"); return emptySchema();}))
        .orElse( null));

    return analyzeSchema( api, schema);
    }    

  /**
   * Updates the DNF of the given parameter schema to normalize the model of valid/invalid values.
   */
  private void normalizeParameterDnf( Parameter parameter, Schema<?> parameterSchema)
    {
    List<Schema<?>> alternatives = 
      Optional.ofNullable( getDnf( parameterSchema))
      .map( Dnf::getAlternatives)
      .orElse( Arrays.asList( parameterSchema));

    for( Schema<?> schema : alternatives)
      {
      normalizeParameterSchema( parameter, schema);
      }
    }

  /**
   * Updates the given parameter schema to normalize the model of valid/invalid values.
   */
  @SuppressWarnings("unchecked")
  private void normalizeParameterSchema( Parameter parameter, Schema<?> parameterSchema)
    {
    // Is this a nullable path parameter?
    if( "path".equals( parameter.getIn()) && Optional.ofNullable( parameterSchema.getNullable()).orElse( false) == true)
      {
      // Yes, null is equivalent to "undefined", which is invalid
      notifyError( "Null values not allowed", "Using nullable=false");
      parameterSchema.setNullable( false);
      }

    // Is this a simple string parameter?
    if( "string".equals( parameterSchema.getType()) && Parameter.StyleEnum.SIMPLE.equals( parameter.getStyle()))
      {
      // Is a valid empty value equivalent to an invalid null value?
      if( Optional.ofNullable( minStringFormat( parameterSchema.getFormat(), parameterSchema.getMinLength(), false)).orElse(0) <= 0
          && Optional.ofNullable( parameterSchema.getNullable()).orElse( false) == false)
        {
        // Yes, avoid inconsistent empty value
        if( parameterSchema.getEnum() != null)
          {
          ((Schema<Object>) parameterSchema).setEnum(
            parameterSchema.getEnum().stream()
            .map( enumValue -> String.valueOf( enumValue).isEmpty()? null : enumValue)
            .collect( toList()));
          }
        else
          {
          notifyWarning( "Empty string values not allowed for non-nullable parameter -- using minLength=1");
          parameterSchema.setMinLength( 1);
          }
        }

      // Is a valid null value equivalent to an invalid empty value?
      else if( Optional.ofNullable( minStringFormat( parameterSchema.getFormat(), parameterSchema.getMinLength(), false)).orElse(0) == 1
               && Optional.ofNullable( parameterSchema.getNullable()).orElse( false) == true)
        {
        // Yes, avoid inconsistent empty value failure
        notifyWarning( "Empty string values allowed for nullable parameter -- using minLength=0");
        parameterSchema.setMinLength( 0);
        }
      }

    // Is this a simple array parameter?
    if( "array".equals( parameterSchema.getType()) && Parameter.StyleEnum.SIMPLE.equals( parameter.getStyle()))
      {
      // Is a valid empty value equivalent to an invalid null value?
      if( Optional.ofNullable( parameterSchema.getMinItems()).orElse(0) <= 0
          && Optional.ofNullable( parameterSchema.getNullable()).orElse( false) == false)
        {
        // Yes, avoid inconsistent empty value
        notifyWarning( "Empty array values not allowed for non-nullable parameter -- using minItems=1");
        parameterSchema.setMinItems( 1);
        }

      // Is a valid null value equivalent to an invalid empty value?
      else if( Optional.ofNullable( parameterSchema.getMinItems()).orElse(0) == 1
               && Optional.ofNullable( parameterSchema.getNullable()).orElse( false) == true)
        {
        // Yes, avoid inconsistent empty value failure
        notifyWarning( "Empty array values allowed for nullable parameter -- using minItems=0");
        parameterSchema.setMinItems( 0);
        }
      }
    }

  /**
   * Returns the media type defined for the given parameter.
   */
  private Optional<MediaType> parameterMediaType( Parameter parameter)
    {
    return
      Optional.ofNullable( parameter.getContent())
      .flatMap( content -> content.values().stream().findFirst());
    }

  /**
   * Returns the example content for the given parameter.
   */
  private Object parameterContentExample( Parameter parameter)
    {
    // The example should be defined either by...
    return
      // ... the parameter itself...
      Optional.ofNullable( parameter.getExample())
      
      .orElseGet( () -> {
        return
          // ... or if no parameter examples defined...
          parameter.getExamples() == null
          // ... by the parameter media type
          ? parameterMediaType( parameter).map( MediaType::getExample).orElse( null)
          : null;
        });
    }

  /**
   * Returns the content examples for the given parameter.
   */
  private Map<String,Example> parameterContentExamples( Parameter parameter)
    {
    // The examples should be defined either by...
    return
      // ... the parameter itself...
      Optional.ofNullable( parameter.getExamples())
      
      .orElseGet( () -> {
        return
          // ... or if no parameter example defined...
          parameter.getExample() == null
          // ... by the parameter media type
          ? parameterMediaType( parameter).map( MediaType::getExamples).orElse( null)
          : null;
        });
    }

  /**
   * Returns the style of the given parameter.
   */
  private Parameter.StyleEnum parameterStyle( Parameter parameter, String parameterType)
    {
    String in = parameter.getIn();
    
    return
      parameter.getStyle() != null?
      getApplicableStyle( parameter, parameterType) :

      in.equals( "path")?
      Parameter.StyleEnum.SIMPLE :

      in.equals( "header")?
      Parameter.StyleEnum.SIMPLE :

      Parameter.StyleEnum.FORM;
    }

  /**
   * If the parameter specifies an applicable style, returns the specified style.
   * Otherwise reports a modelling condition and returns a default applicable style.
   */
  private Parameter.StyleEnum getApplicableStyle( Parameter parameter, String parameterType)
    {
    try
      {
      return ifApplicableStyle( parameter.getStyle(), parameter.getIn(), parameterType);
      }
    catch( InvalidStyleException e)
      {
      notifyError( e.getMessage(), String.format( "Using style=%s instead", e.getValidStyle()));
      return Parameter.StyleEnum.valueOf( e.getValidStyle().toUpperCase());
      }
    }

  /**
   * Resolves the value of a parameter explode property.
   */
  private Boolean parameterExplode( Boolean explode, Set<String> parameterTypes, Parameter.StyleEnum parameterStyle)
    {
    return
      parameterTypes != null && !(parameterTypes.contains( "object") || parameterTypes.contains( "array"))?
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
  private IVarDef parameterDefinedVar( String parameterVarTag, Parameter parameter)
    {
    return
      VarDefBuilder.with( instanceDefinedVar( parameterVarTag, Boolean.TRUE.equals( parameter.getRequired())))
      .has( "style", parameter.getStyle())
      .has( "explode", parameter.getExplode())
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
          .members( headerDefinedVar( headerName, headerVarTag, header))
          .members( instanceSchemaVars( headerVarTag, headerSchema))
          .build();
        });
    }

  /**
   * Returns an {@link IVarDef input variable} to represent if the given header is defined.
   */
  private IVarDef headerDefinedVar( String headerName, String headerVarTag, Header header)
    {
    return
      VarDefBuilder.with( instanceDefinedVar( headerVarTag, Boolean.TRUE.equals( header.getRequired())))
      .has( "headerName", headerName)
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
        ifNotEmpty( response.getContent())
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
   * Returns the URI for the test API server.
   */
  private Optional<String> serverUriUsed( List<Server> servers)
    {
    Optional<String> specified =
      Optional.ofNullable( getOptions().getServerUri())
      .map( String::valueOf);

    return
      specified.isPresent()?
      specified :
      
      getOptions().getServerSelector().select(
        membersOf( servers)
        .map( Server::getDescription)
        .collect( toList()))
      .map( i -> getServerUrl( servers.get(i)));
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
    return instanceSchemaVars( instanceVarTag, instanceOptional, instanceSchema, false);
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> instanceSchemaVars( String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema, boolean instanceItem)
    {
    String instanceType = Optional.ofNullable( instanceSchema).map( Schema::getType).orElse( null);
    return
      // Missing schema (equivalent to an empty schema)?
      instanceSchema == null?
      Stream.of( instanceTypeVar( instanceVarTag, instanceOptional, instanceSchema, instanceItem)) :

      // Unknown schema type?
      !isSchemaType( instanceType)?
      unknownSchemaVars( instanceType) :

      // No, return all input variables for this schema
      allSchemaVars( instanceType, instanceVarTag, instanceOptional, instanceSchema, instanceItem);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by every alternative schema for the given instance.
   */
  private Stream<IVarDef> allSchemaVars( String instanceType, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema, boolean instanceItem)
    {
    List<Schema<?>> alternatives =
      Optional.ofNullable( instanceSchema)
      .flatMap( s -> Optional.ofNullable( getDnf( s)))
      .map( dnf -> simplified( dnf).getAlternatives())
      .orElse( emptyList());

    return
      alternatives.isEmpty()?
      typeSchemaVars( instanceType, instanceVarTag, instanceOptional, instanceSchema, instanceItem) :
      
      alternatives.size() == 1?
      typeSchemaVars( instanceVarTag, instanceOptional, alternatives.get(0), instanceItem) :

      alternativeSchemaVars( instanceVarTag, instanceOptional, alternatives, instanceItem);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by the given alternative schemas for the given instance.
   */
  private Stream<IVarDef> alternativeSchemaVars( String instanceVarTag, boolean instanceOptional, List<Schema<?>> alternatives, boolean instanceItem)
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
              .members( typeSchemaVars( instanceVarTag, instanceOptional, alternatives.get(i), instanceItem))
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
  private Stream<IVarDef> typeSchemaVars( String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema, boolean instanceItem)
    {
    return typeSchemaVars( instanceSchema.getType(), instanceVarTag, instanceOptional, instanceSchema, instanceItem);
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private Stream<IVarDef> typeSchemaVars( String instanceType, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema, boolean instanceItem)
    {
    if( isModellingInput( instanceSchema))
      {
      throw new IllegalStateException( "Can't create an input model for a schema that references itself");
      }
    
    try
      {
      setModellingInput( instanceSchema, true);
    
      Stream.Builder<IVarDef> typeVars = Stream.builder();

      typeVars.add( instanceTypeVar( instanceVarTag, instanceOptional, instanceSchema, instanceItem));
      if( instanceType != null)
        {
        typeVars.add( typeValueVar( instanceType, instanceVarTag, instanceSchema, instanceItem));
        }

      return typeVars.build();
      }
    finally
      {
      setModellingInput( instanceSchema, false);
      }
    }
  
  /**
   * Returns the type-specific {@link IVarDef input variable} for the value defined by the given instance schema.
   */
  private IVarDef typeValueVar( String instanceType, String instanceVarTag, Schema<?> instanceSchema, boolean instanceItem)
    {
    return
      "object".equals( instanceType)?
      objectValueVar( instanceVarTag, instanceSchema, instanceItem) :
      
      "string".equals( instanceType)?
      stringValueVar( instanceVarTag, instanceSchema, instanceItem) :
      
      "integer".equals( instanceType)?
      numberValueVar( instanceVarTag, instanceSchema, instanceItem) :
      
      "boolean".equals( instanceType)?
      booleanValueVar( instanceVarTag, instanceSchema, instanceItem) :

      "array".equals( instanceType)?
      arrayValueVar( instanceVarTag, instanceSchema, instanceItem) :
      
      // "number".equals( instanceType)
      numberValueVar( instanceVarTag, instanceSchema, instanceItem);
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
  private IVarDef arrayValueVar( String instanceVarTag, Schema<?> instanceSchema, boolean instanceItem)
    {
    IVarDef valueVar;

    Set<?> enums = nullableEnums( asOrderedSet( instanceSchema.getEnum()), instanceSchema.getNullable());
    if( !enums.isEmpty())
      {
      valueVar = 
        VarDefBuilder.with( "Value")
        .hasIf( "itemEnums", Optional.of( enums).filter( e -> instanceItem).orElse( null))
        .when( has( instanceValueProperty( instanceVarTag)))
        .values( enums.stream().map( i -> VarValueDefBuilder.with( i).build()))
        .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).has( "excluded", enums).build())
        .build();
      }
    else
      {
      Schema<?> itemSchema =
        Optional.ofNullable( asArraySchema( instanceSchema))
        .map( array -> array.getItems())
        .orElse( null);

      Optional<IVarDef> itemsVar = arrayItemsVar( instanceVarTag, instanceSchema, itemSchema);

      // If the item schema defines a maximum number of possible values...
      Optional<Integer> maxUniqueItems =
        Optional.ofNullable( itemSchema)
        .flatMap( s -> itemsVar)
        .flatMap( items -> Optional.ofNullable( getMaxAlternativeValues( itemSchema)));

      // ... and the items must be unique...
      boolean uniqueRequired = Boolean.TRUE.equals( instanceSchema.getUniqueItems());
      boolean failBelowMinItems = true;
      boolean failAboveMaxItems = true;
      if( uniqueRequired)
        {
        // ... then, if necessary, adjust "minItems"
        if( maxUniqueItems
            .map( maxUnique -> Optional.ofNullable( instanceSchema.getMinItems()).map( minItems -> minItems > maxUnique).orElse( true))
            .orElse( false))
          {
          notifyError(
            String.format( "minItems=%s can exceed the number of unique item values possible", instanceSchema.getMinItems()),
            String.format( "Adjusting to unique minItems=%s", maxUniqueItems.get()));

          instanceSchema.setMinItems( maxUniqueItems.get());
          failBelowMinItems = false;
          }

        // ... and, if necessary, adjust "maxItems"
        if( maxUniqueItems
            .map( maxUnique -> Optional.ofNullable( instanceSchema.getMaxItems()).map( maxItems -> maxItems > maxUnique).orElse( true))
            .orElse( false))
          {
          notifyError(
            String.format( "maxItems=%s can exceed the number of unique item values possible", instanceSchema.getMaxItems()),
            String.format( "Adjusting to unique maxItems=%s", maxUniqueItems.get()));

          instanceSchema.setMaxItems( maxUniqueItems.get());
          failAboveMaxItems = false;
          }
        }

      valueVar =
        VarSetBuilder.with( "Items")
        .when( has( instanceValueProperty( instanceVarTag)))
        .members( arraySizeVar( instanceVarTag, instanceSchema, instanceItem, failBelowMinItems, failAboveMaxItems))
        .members( iterableOf( itemsVar))
        .members( iterableOf( arrayUniqueItemsVar( instanceVarTag, instanceSchema)))
        .build();
      }

    return valueVar;
    }

  /**
   * Returns the {@link VarDef input variable} representing the size of an array instance.
   */
  private VarDef arraySizeVar( String instanceVarTag, Schema<?> arraySchema, boolean instanceItem, boolean failBelowMinItems, boolean failAboveMaxItems)
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
        if( failBelowMinItems)
          {
          sizeValues.add( minItems - 1);
          }
        sizeValues.add( minItems);
        }
      if( maxItems != null)
        {
        sizeValues.add( maxItems);
        if( failAboveMaxItems)
          {
          sizeValues.add( maxItems + 1);
          }
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
            .has( "rangeMin", 1)
            .properties( Optional.ofNullable( maxItems > 2? arrayItemsManyProperty( instanceVarTag) : null))
            .build());
          }
        }
      }

    if( instanceItem)
      {
      size.hasIf( "itemMinItems", minItems);
      size.hasIf( "itemMaxItems", maxItems);
      }

    boolean uniqueRequired = Boolean.TRUE.equals( arraySchema.getUniqueItems());
    if( uniqueRequired)
      {
      size.has( "itemsUnique", true);
      }
    
    return size.build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the items of the given array instance.
   */
  private Optional<IVarDef> arrayItemsVar( String instanceVarTag, Schema<?> arraySchema, Schema<?> itemSchema)
    {
    return
      Optional.ofNullable( arraySchema.getMaxItems()).map( maxItems -> maxItems <= 0).orElse( false)?
      Optional.empty() :
      
      Optional.of(
        resultFor( "items", () -> {
          boolean allowSizeZero = Optional.ofNullable( arraySchema.getMinItems()).map( min -> min == 0).orElse( true);
          ICondition itemsCondition = allowSizeZero? not( arrayItemsNoneProperty( instanceVarTag)) : null;

          return
            VarSetBuilder.with( "Contains")
            .when( itemsCondition)
            .members( instanceSchemaVars( arrayItemsProperty( instanceVarTag), false, itemSchema, true))
            .build();
          }));
    }

  /**
   * Returns the {@link IVarDef input variable} representing the unique items property of an array instance.
   */
  private Optional<IVarDef> arrayUniqueItemsVar( String instanceVarTag, Schema<?> arraySchema)
    {
    boolean uniqueRequired = Boolean.TRUE.equals( arraySchema.getUniqueItems());

    return
      Optional.ofNullable( arraySchema.getMaxItems()).map( maxItems -> maxItems <= 1).orElse( false)?
      Optional.empty() :

      Optional.of(
        VarDefBuilder.with( "Unique")
        .when( has( arrayItemsManyProperty( instanceVarTag)))
        .values(
          VarValueDefBuilder.with( "Yes").build(),
          VarValueDefBuilder.with( "No").type( uniqueRequired? VarValueDef.Type.FAILURE: VarValueDef.Type.VALID).build())
        .build());
    }   

  /**
   * Returns the {@link IVarDef input variable} representing the type of a instance.
   */
  private IVarDef instanceTypeVar( String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema, boolean instanceItem)
    {
    Optional<Schema<?>> schema = Optional.ofNullable( instanceSchema);
    Boolean nullable = schema.map( s -> s.getNullable()).orElse( Boolean.FALSE);
    boolean nullChecked =  schema.map( s -> isNullChecked( s)).orElse( true);

    Optional<String> type = schema.flatMap( s -> Optional.ofNullable( s.getType()));
    boolean typeChecked =  schema.map( s -> isTypeChecked( s)).orElse( true);
    Set<String> notTypes = schema.flatMap( s -> Optional.ofNullable( getNotTypes( s))).orElse( emptySet());
    Set<String> requiredTypes = schema.flatMap( s -> Optional.ofNullable( getRequiredTypes( s))).orElse( emptySet());

    String validTypes = type.orElse( String.format( "Not %s", notTypes.isEmpty()? "null" : notTypes.stream().collect( joining( ","))));

    return
      VarDefBuilder.with( "Type")
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .hasIf( "itemType", Optional.of( validTypes).filter( itemType -> instanceItem))

      // Expectations for valid types
      .values(
        VarValueDefBuilder.with( validTypes)
        .properties( instanceValueProperty( instanceVarTag)).build())

      // Expectations for null type
      .values(
        streamOf(
          Optional.of( nullChecked)
          .filter( checked -> checked)
          .map(
            checked ->
            VarValueDefBuilder.with( (Object) null).type( Boolean.TRUE.equals( nullable)? VarValueDef.Type.ONCE : VarValueDef.Type.FAILURE).build())))

      // Expectations for invalid types
      .values(
        (typeChecked?

         type
         .map( t -> Stream.of( String.format( "Not %s", requiredTypes.stream().collect( joining( ",")))))
         .orElse( notTypes.stream()):

         Stream.empty())
        .map( invalidType -> VarValueDefBuilder.with( invalidType).type( VarValueDef.Type.FAILURE).build()))

      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a number instance.
   */
  private IVarDef numberValueVar( String instanceVarTag, Schema<?> instanceSchema, boolean instanceItem)
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
    BigDecimal effectiveMaximum = null;
    BigDecimal effectiveMinimum = null;

    // Enumerated values?
    Set<BigDecimal> enums = nullableEnums( getNumberEnum( instanceSchema), instanceSchema.getNullable());
    Set<BigDecimal> notEnums = getNumberEnum( getNotEnums( instanceSchema));
    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      quantity.values( enums.stream().map( i -> VarValueDefBuilder.with( i).build()));
      quantity.values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).has( "excluded", enums).build());
      setMaxValues( instanceSchema, enums.size());
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
      effectiveMaximum = maximum;
      effectiveMinimum = 
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

      // For any missing boundary, add a value designating an unbounded range
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

          // Maximum values in this bounded range can be defined.
          setMaxValues( instanceSchema, multiplesBetween( effectiveMinimum, maximum, multipleOf, notMultipleOfs));
          }

        BigDecimal nextMultiple = effectiveMinimum.add( effectiveMultipleOf);
        notMultipleOfs.stream()
          .forEach(
            m -> {
            // Select a value within bounds that fails each not-multiple-of constraint.
            BigDecimal notMultipleOfFailure;
            for( notMultipleOfFailure = nextMultiple;
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

    // Does this schema define the items of an array?
    if( instanceItem)
      {
      // Yes, annotate with constraints that apply to all array items
      if( !enums.isEmpty())
        {
        quantity.has( "itemEnums", enums);
        }
      else
        {
        quantity.hasIf( "itemMin", effectiveMinimum);
        quantity.hasIf( "itemMax", effectiveMaximum);
        quantity.hasIf( "itemNotEnums", notEnums);
        quantity.hasIf( "itemMultipleOf", multipleOf);
        quantity.hasIf( "itemNotMultipleOfs", notMultipleOfs);
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
              VarValueDefBuilder.with( "No").has( "multipleOf", multipleOf).type( VarValueDef.Type.FAILURE).build())
            .build());
        }
      for( BigDecimal m : notMultipleOfs)
        {
        multiples.members
          ( VarDefBuilder.with( toIdentifier( m))
            .values(
              VarValueDefBuilder.with( "Yes").type( VarValueDef.Type.FAILURE).has( "multipleOf", m).build(),
              VarValueDefBuilder.with( "No").has( "multipleOf", m).build())
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
  private IVarDef objectValueVar( String instanceVarTag, Schema<?> instanceSchema, boolean instanceItem)
    {
    IVarDef valueVar;

    Set<?> enums = nullableEnums( asOrderedSet( instanceSchema.getEnum()), instanceSchema.getNullable());
    if( !enums.isEmpty())
      {
      valueVar = 
        VarDefBuilder.with( "Value")
        .hasIf( "itemEnums", Optional.of( enums).filter( e -> instanceItem).orElse( null))
        .when( has( instanceValueProperty( instanceVarTag)))
        .values( enums.stream().map( i -> VarValueDefBuilder.with( i).build()))
        .values( VarValueDefBuilder.with( "Other").type( VarValueDef.Type.FAILURE).has( "excluded", enums).build())
        .build();
      }
    else
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

      valueVar =
        VarSetBuilder.with( "Value")
        .when( has( instanceValueProperty( instanceVarTag)))
        .members( iterableOf( objectPropertyCountVar( instanceVarTag, instanceSchema, constraints)))
        .members( objectPropertiesVar( instanceVarTag, instanceSchema, constraints))
        .build();
      }

    return valueVar;
    }

  /**
   * Returns a new schema that validates the examples specified for the given instance.
   */
  private Schema<?> exampleSchemaFor( Object instanceExample, Map<String,Example> instanceExamples, Schema<?> instanceSchema)
    {
    String exampleType = Optional.ofNullable( instanceSchema).map( Schema::getType).orElse( null);
    boolean exampleNullable = Optional.ofNullable( instanceSchema).flatMap( s -> Optional.ofNullable( s.getNullable())).orElse( false);

    Optional<Object> exampleObject = Optional.ofNullable( instanceExample);

    return
      exampleObject.isPresent()?
      exampleSchemaFor( exampleObject.get(), exampleType, exampleNullable) :

      !Optional.ofNullable( instanceExamples).map( Map::isEmpty).orElse( true)?
      exampleSchemaFor(
        instanceExamples.values().stream().map( Example::getValue).collect( toList()),
        exampleType,
        exampleNullable) :

      exampleSchemaFor( instanceSchema, exampleType, exampleNullable);
    }

  /**
   * Returns a new schema that validates examples described by the given schema.
   */
  private Schema<?> exampleSchemaFor( Schema<?> instanceSchema)
    {
    return
      exampleSchemaFor(
        instanceSchema,
        Optional.ofNullable( instanceSchema).map( Schema::getType).orElse( null),
        Optional.ofNullable( instanceSchema).flatMap( s -> Optional.ofNullable( s.getNullable())).orElse( false));
    }

  /**
   * Returns a new schema that validates examples described by the given schema.
   */
  private Schema<?> exampleSchemaFor( Schema<?> instanceSchema, String exampleType, boolean exampleNullable)
    {
    Optional<Object> exampleObject;
    Set<Object> exampleEnum;

    return
      instanceSchema == null?
      null :

      !(exampleEnum = exampleEnum( instanceSchema)).isEmpty()?
      exampleSchemaForEnum( exampleEnum, exampleType, exampleNullable) :
      
      (exampleObject = schemaExample( instanceSchema)) != null?
      exampleSchemaFor( exampleObject.orElse( null), exampleType, exampleNullable) :

      derivedExampleSchemaFor( instanceSchema, exampleType);
    }

  /**
   * Returns a new schema that validates only the given example value.
   */
  private Schema<?> exampleSchemaFor( Object exampleValue, String exampleType, boolean exampleNullable)
    {
    Set<Object> exampleEnum = nullableEnums( asOrderedSet( Arrays.asList( exampleValue)), exampleNullable);
    return exampleSchemaForEnum( exampleEnum, exampleType, exampleNullable);
    }

  /**
   * Returns a new schema that validates only the given example values.
   */
  @SuppressWarnings("rawtypes")
  private Schema<?> exampleSchemaFor( List<Object> exampleValues, String exampleType, boolean exampleNullable)
    {
    Map<String,List<Object>> exampleEnums =
      nullableEnums( asOrderedSet( exampleValues), exampleNullable)
      .stream()
      .collect( groupingBy( v -> String.valueOf( exampleTypeOf( v))));

    List<Schema> exampleSchemas =
      exampleEnums.keySet().stream()
      .filter( type -> {
          boolean typeValid = exampleType == null || exampleType.equals( type);
          if( !typeValid)
            {
            notifyError(
              String.format(
                "Expecting example values of type=%s, but found values=%s",
                exampleType,
                exampleEnums.get( type)),
              "Ignoring unexpected example values");
            }
          return typeValid;
        })
      .map( type -> exampleSchemaForEnum( asOrderedSet( exampleEnums.get( type)), type, exampleNullable))
      .collect( toList());

    return
      Optional.of( exampleSchemas)
      .filter( s -> !s.isEmpty())
      .map( s -> s.size() == 1 ? s.iterator().next() : new ComposedSchema().oneOf( s))
      .orElseThrow( () -> new IllegalStateException( "No example values defined"));
    }

  /**
   * Returns a new schema that validates only the given example values.
   */
  @SuppressWarnings("unchecked")
  private Schema<?> exampleSchemaForEnum( Set<Object> exampleValues, String exampleType, boolean exampleNullable)
    {
    Schema<Object> exampleSchema;
    if( exampleValues.isEmpty())
      {
      exampleSchema = null;
      }
    else
      {
      exampleSchema = new Schema<Object>().type( exampleType).nullable( exampleNullable);
      exampleSchema.setEnum( exampleValues.stream().collect( toList()));
      }
    
    return exampleSchema;
    }

  /**
   * Returns the schema type for the given example value
   */
  private String exampleTypeOf( Object exampleValue)
    {
    return
      exampleValue == null?
      null :

      exampleValue instanceof String?
      "string" :

      (exampleValue instanceof Integer || exampleValue instanceof Long)?
      "integer" :

      exampleValue instanceof Number?
      "number" :

      exampleValue instanceof Boolean?
      "boolean" :

      exampleValue instanceof ArrayNode?
      "array" :

      "object";
    }

  /**
   * Returns the enumerated values for the given schema.
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  private Set<Object> exampleEnum( Schema instanceSchema)
    {
    Set<Object> instanceEnum = nullableEnums( asOrderedSet( instanceSchema.getEnum()), instanceSchema.getNullable());
    
    return
      !instanceEnum.isEmpty()?
      instanceEnum :

      "boolean".equals( instanceSchema.getType())?
      asOrderedSet( Arrays.asList( Boolean.TRUE, Boolean.FALSE)) :

      emptySet();
    }

  /**
   * If example data can be derived for the <CODE>instanceSchema</CODE>, returns a new schema that
   * validates example data. Otherwise, returns the <CODE>instanceSchema</CODE>.
   */
  @SuppressWarnings("rawtypes")
  private Schema<?> derivedExampleSchemaFor( Schema<?> instanceSchema, String instanceType)
    {
    Schema<?> exampleSchema;

    try
      {
      if( instanceSchema.getNot() != null)
        {
        throw exampleException( "'not' assertion defined");
        }
      
      ComposedSchema composed = asComposedSchema( instanceSchema);
      if( composed != null)
        {
        if( !composed.getAllOf().isEmpty())
          {
          throw exampleException( "'allOf' assertion defined");
          }

        boolean anyOf = !composed.getAnyOf().isEmpty();
        boolean oneOf = !composed.getOneOf().isEmpty();
        if( anyOf && oneOf)
          {
          throw exampleException( "both 'anyOf' and 'oneOf' assertions defined");
          }

        if( !isLeafEmpty( composed))
          {
          throw exampleException( String.format( "if '%s' defined, no other assertions allowed", oneOf? "oneOf" : "anyOf"));
          }

        List<Schema> members = oneOf? composed.getOneOf() : composed.getAnyOf();
        exampleSchema =
          new ComposedSchema()
          .oneOf(
            IntStream.range( 0, members.size())
            .mapToObj( i -> resultFor( String.format( "%s[%s]", oneOf? "oneOf" : "anyOf", i), () -> exampleSchemaFor( members.get(i)))) 
            .collect( toList()));
        }

      else if( "array".equals( instanceType))
        {
        exampleSchema =
          Optional.of( asArraySchema( copySchema( instanceSchema)))
          .map( s -> s.items( resultFor( "items", () -> exampleSchemaFor( s.getItems()))))
          .orElseThrow( () -> new IllegalStateException( "Can't compose array schema examples"));
        }

      else if( "object".equals( instanceType)
               &&
               !(Optional.ofNullable( instanceSchema.getProperties()).orElse( emptyMap()).isEmpty()
                 && additionalPropertiesSchema( instanceSchema) == null))
        {
        exampleSchema =
          copySchema( instanceSchema)

          .properties(
            Optional.ofNullable( instanceSchema.getProperties()).orElse( emptyMap())
            .entrySet().stream()
            .collect( toMap( e -> e.getKey(), e -> resultFor( e.getKey(), () -> exampleSchemaFor( e.getValue())), (s1,s2) -> s1, LinkedHashMap::new)))

          .additionalProperties(
            Optional.ofNullable( additionalPropertiesSchema( instanceSchema))
            .map( s -> resultFor( "additionalProperties", () -> (Object) exampleSchemaFor( s)))
            .orElse( instanceSchema.getAdditionalProperties()));
        }

      else if( instanceSchema.getDefault() != null)
        {
        exampleSchema = exampleSchemaFor( instanceSchema.getDefault(), instanceType, Optional.ofNullable( instanceSchema.getNullable()).orElse( false));
        }

      else
        {
        throw exampleException( String.format( "no example defined for schema of type=%s", instanceType));
        }
      }
    catch( ExampleException e)
      {
      notifyWarning(
        String.format(
          "Example data unavailable -- %s. Inputs will be defined by schema",
          e.getMessage()));

      exampleSchema = instanceSchema;
      }

    return exampleSchema;
    }

  /**
   * Returns a new {@link ExampleException}.
   */
  private ExampleException exampleException( String reason)
    {
    return new ExampleException( getContext().getLocation(), reason);
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
          .has( "excludedType", Optional.ofNullable( propertySchema.getType()).orElse( "Not null"))
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

    Schema<?> propertySchema = additionalPropertiesSchema( instanceSchema);

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
  private IVarDef stringValueVar( String instanceVarTag, Schema<?> instanceSchema, boolean instanceItem)
    {
    IVarDef valueVar;

    // Enumerated values?
    String format = instanceSchema.getFormat();

    Set<FormattedString> enums =
      getFormattedStrings(
        "enumerated",
        format,
        nullableEnums( asOrderedSet( instanceSchema.getEnum()), instanceSchema.getNullable()));

    if( !enums.isEmpty())
      {
      // Yes, add valid and invalid values for this enumeration
      valueVar = 
        VarDefBuilder.with( "Value")
        .has( "format", format)
        .has( "default", Objects.toString( getFormattedString( "default", format, instanceSchema.getDefault()), null))
        .hasIf( "itemEnums", Optional.of( enums).filter( e -> instanceItem).orElse( null))
        .when( has( instanceValueProperty( instanceVarTag)))
        .values( enums.stream().map( i -> VarValueDefBuilder.with( String.valueOf( i)).build()))
        .values( VarValueDefBuilder.with( stringNotEnumerated( enums)).type( VarValueDef.Type.FAILURE).has( "excluded", enums).build())
        .build();

      setMaxValues( instanceSchema, enums.size());
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
      Integer maxLength = maxPatternMatch( instanceSchema, maxStringFormat( format, instanceSchema.getMaxLength()));

      Integer minLength = 
        Optional.ofNullable( minPatternMatch( instanceSchema, minStringFormat( format, instanceSchema.getMinLength())))
        .map( min -> Optional.ofNullable( maxLength).map( max -> adjustedMinOf( "Length", min, max)).orElse( min))
        .orElse( null);

      // Get any excluded values (ignoring any with invalid length)
      Set<FormattedString> notEnums =
        getFormattedStrings( "enumerated", format, getNotEnums( instanceSchema))
        .stream()
        .filter(
          excluded -> {
            int formattedLength = excluded.toString().length();
            return !(minLength != null && formattedLength < minLength) && !(maxLength != null && formattedLength > maxLength);
          })
        .collect( toOrderedSet());
      Optional<String> notExcludedProperty = Optional.of( valueNotExcludedProperty( instanceVarTag)).filter( p -> !notEnums.isEmpty());
      Optional<ICondition> notExcluded = notExcludedProperty.map( p -> has( p));
      
      // Add values for any length assertions
      VarDefBuilder length = VarDefBuilder.with( "Length").when( notExcluded);

      // Any length assertions specified?
      if( minLength == null && maxLength == null)
        {
        // No, add standard values
        length.values(
          VarValueDefBuilder.with( "> 0")
          .properties( validLengthProperty( instanceVarTag))
          .properties( notEmptyProperty( instanceVarTag))
          .build());

        if(
          // Format allows empty string?
          (format == null || format.equals( "byte") || format.equals( "binary") || format.equals( "password"))
          &&
          // Empty string not excluded?
          !notEnums.stream().map( String::valueOf).anyMatch( String::isEmpty))
          {
          // Yes, allow empty values
          length.values(
            VarValueDefBuilder.with( 0)
            .properties( validLengthProperty( instanceVarTag))
            .build());
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
            VarValueDef.Type type =
              (i < effMinLength) || (maxLength != null && i > maxLength)
              ? VarValueDef.Type.FAILURE
              : VarValueDef.Type.VALID;

            Optional<String> validLength =
              Optional.of( validLengthProperty( instanceVarTag))
              .filter( p -> type == VarValueDef.Type.VALID);

            Optional<String> notEmpty =
              validLength
              .filter( p -> i > 0)
              .map( p -> notEmptyProperty( instanceVarTag));
            
            length.values(
              VarValueDefBuilder.with( i)
              .type( type)
              .properties( validLength)
              .properties( notEmpty)
              .build());
            }
          }
        if( minLength == null)
          {
          if( maxLength > 1)
            {
            length.values(
              VarValueDefBuilder.with( String.format( "< %s", maxLength))
              .has( "rangeMin", 1)
              .properties( validLengthProperty( instanceVarTag))
              .properties( notEmptyProperty( instanceVarTag))
              .build());
            }
          }
        else if( maxLength == null)
          {
          length.values(
            VarValueDefBuilder.with( String.format( "> %s", minLength))
            .properties( validLengthProperty( instanceVarTag))
            .properties( notEmptyProperty( instanceVarTag))
            .build());
          }
        }

      // Does this schema define the items of an array?
      if( instanceItem)
        {
        length.hasIf( "itemMinLength", Optional.ofNullable( minLength).orElse(0));
        length.hasIf( "itemMaxLength", maxLength);
        length.hasIf( "itemNotEnums", notEnums);
        length.hasIf( "itemPatterns", getPatterns( instanceSchema));
        length.hasIf( "itemNotPatterns", getNotPatterns( instanceSchema));
        }

      VarDef lengthVar = length.build();
      valueVarSet.members( lengthVar);

      // Add variables for any pattern assertions
      String[] patterns = getPatterns( instanceSchema).stream().toArray(String[]::new);
      String[] notPatterns = Optional.ofNullable( getNotPatterns( instanceSchema)).orElse( emptySet()).stream().toArray(String[]::new);

      Optional<ICondition> notEmpty =
        toStream( lengthVar.getValidValues())
        .filter( value -> value.hasProperty( notEmptyProperty( instanceVarTag)))
        .findFirst()
        .map( value -> has( notEmptyProperty( instanceVarTag)));

      if( patterns.length == 1 && notPatterns.length == 0)
        {
        valueVarSet.members(
          VarDefBuilder.with( "Matches-Pattern")
          .when( has( validLengthProperty( instanceVarTag)))
          .has( "pattern", patterns[0])
          .values(
            VarValueDefBuilder.with( "Yes").build(),
            VarValueDefBuilder.with( "No").when( notEmpty).type( VarValueDef.Type.FAILURE).build())
          .build());
        }
      else if( patterns.length == 0 && notPatterns.length == 1)
        {
        valueVarSet.members(
          VarDefBuilder.with( "Matches-Pattern")
          .when( has( validLengthProperty( instanceVarTag)))
          .has( "pattern", notPatterns[0])
          .values(
            VarValueDefBuilder.with( "Yes").when( notEmpty).type( VarValueDef.Type.FAILURE).build(),
            VarValueDefBuilder.with( "No").build())
          .build());
        }
      else if( patterns.length > 0 || notPatterns.length > 0)
        {
        valueVarSet.members(
          VarSetBuilder.with( "Matches-Patterns")
          .when( has( validLengthProperty( instanceVarTag)))
          .members(
            IntStream.range( 0, patterns.length)
            .mapToObj(
              i ->
              VarDefBuilder.with( String.valueOf(i))
              .has( "pattern", patterns[i])
              .values(
                VarValueDefBuilder.with( "Yes").build(),
                VarValueDefBuilder.with( "No").when( notEmpty).type( VarValueDef.Type.FAILURE).build())
              .build()))
          .members(
            IntStream.range( 0, notPatterns.length)
            .mapToObj(
              i ->
              VarDefBuilder.with( String.valueOf( patterns.length + i))
              .has( "pattern", notPatterns[i])
              .values(
                VarValueDefBuilder.with( "Yes").when( notEmpty).type( VarValueDef.Type.FAILURE).build(),
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
  private IVarDef booleanValueVar( String instanceVarTag, Schema<?> instanceSchema, boolean instanceItem)
    {
    Set<Boolean> possibleValues = asOrderedSet( Boolean.TRUE, Boolean.FALSE);
    Set<Boolean> excludedValues = getBooleanEnum( getNotEnums( instanceSchema));
    Set<Boolean> enumValues = nullableEnums( getBooleanEnum( instanceSchema), instanceSchema.getNullable());

    List<Boolean> allowedValues =
      Optional.of( enumValues)
      .filter( enums -> !enums.isEmpty())
      .orElse( possibleValues)
      .stream()
      .filter( b -> !excludedValues.contains( b))
      .collect( toList());

    if( allowedValues.isEmpty())
      {
      throw new IllegalStateException( "All possible boolean values have been excluded by this schema");
      }
    setMaxValues( instanceSchema, allowedValues.size());
    
    return
      VarDefBuilder.with( "Value")
      .has( "default", Objects.toString( instanceSchema.getDefault(), null))
      .hasIf( "itemEnums", Optional.of( allowedValues).filter( e -> instanceItem).orElse( null))
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
   * Returns the given set of enumerated values, excluding any null value.
   */
  private <T> Set<T> nullableEnums( Set<T> enums, Boolean instanceNullable)
    {
    if( enums.contains( null) && !Optional.ofNullable( instanceNullable).orElse( false))
      {
      notifyWarning( "'null' is not a valid enumerated value for a non-nullable schema");
      }

    enums.remove( null);
    return enums;
    }

  /**
   * If the given <CODE>maxLength</CODE> is valid for a string in the given format, returns <CODE>maxLength</CODE>.
   * Otherwise, returns the format-specific maximum length.
   */
  private Integer maxStringFormat( String format, Integer maxLength)
    {
    Integer max;
    Integer maxAllowed = stringFormatMax( format);

    if( maxAllowed == null)
      {
      max = maxLength;
      }
    else if( maxLength == null)
      {
      max = maxAllowed;
      }
    else if( maxLength <= maxAllowed)
      {
      max = maxLength;
      }
    else
      {
      notifyWarning(
        String.format(
          "maxLength=%s exceeds maximum allowed for format=%s -- using maxLength=%s instead",
          maxLength,
          format,
          maxAllowed));
      max = maxAllowed;
      }

    return max;
    }

  /**
   * If the given <CODE>minLength</CODE> is valid for a string in the given format, returns <CODE>minLength</CODE>.
   * Otherwise, returns the format-specific minimum length.
   */
  private Integer minStringFormat( String format, Integer minLength)
    {
    return minStringFormat( format, minLength, true);
    }

  /**
   * If the given <CODE>minLength</CODE> is valid for a string in the given format, returns <CODE>minLength</CODE>.
   * Otherwise, returns the format-specific minimum length. If <CODE>notify</CODE> is true, reports a warning
   * if the original was invalid.
   */
  private Integer minStringFormat( String format, Integer minLength, boolean notify)
    {
    Integer min;
    Integer minAllowed = stringFormatMin( format);

    if( minAllowed == null)
      {
      min = minLength;
      }
    else if( minLength == null)
      {
      min = minAllowed;
      }
    else if( minLength >= minAllowed)
      {
      min = minLength;
      }
    else
      {
      if( notify)
        {
        notifyWarning(
          String.format(
            "minLength=%s is below the minimum allowed for format=%s -- using minLength=%s instead",
            minLength,
            format,
            minAllowed));
        }
      min = minAllowed;
      }

    return min;
    }

  /**
   * If the given <CODE>maxLength</CODE> is valid for values matching the patterns required for the
   * given schema, returns <CODE>maxLength</CODE>.  Otherwise, returns the maximum
   * length required for pattern matches.
   */
  private Integer maxPatternMatch( Schema<?> stringSchema, Integer maxLength)
    {
    Integer max;

    Integer maxAllowed =
      patternGenerators( stringSchema)
      .map( RegExpGen::getMaxLength)
      .min( Integer::compareTo)
      .map( matchMax -> bounded( matchMax).orElse( null))
      .orElse( null);

    if( maxAllowed == null)
      {
      max = maxLength;
      }
    else if( maxLength == null)
      {
      max = maxAllowed;
      }
    else if( maxLength <= maxAllowed)
      {
      max = maxLength;
      }
    else
      {
      notifyWarning(
        String.format(
          "maxLength=%s exceeds maximum allowed for pattern matches -- using maxLength=%s instead",
          maxLength,
          maxAllowed));
      max = maxAllowed;
      }

    return max;
    }

  /**
   * If the given <CODE>minLength</CODE> is valid for values matching the patterns required for the
   * given schema, returns <CODE>minLength</CODE>.  Otherwise, returns the minimum
   * length required for pattern matches.
   */
  private Integer minPatternMatch( Schema<?> stringSchema, Integer minLength)
    {
    Integer min;

    Integer minAllowed =
      patternGenerators( stringSchema)
      .map( RegExpGen::getMinLength)
      .max( Integer::compareTo)
      .orElse( null);

    if( minAllowed == null)
      {
      min = minLength;
      }
    else if( minLength == null)
      {
      min = minAllowed;
      }
    else if( minLength >= minAllowed)
      {
      min = minLength;
      }
    else
      {
      notifyWarning(
        String.format(
          "minLength=%s is below the minimum allowed for pattern matches -- using minLength=%s instead",
          minLength,
          minAllowed));
      min = minAllowed;
      }

    return min;
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
   * Return the largest number less than (or, if inclusive, equal to) the given value that satisfies the given (not-)multiple-of constraints.
   */
  private BigDecimal multipleBelow( BigDecimal value, boolean inclusive, BigDecimal multipleOf, Set<BigDecimal> notMultipleOfs)
    {
    BigDecimal below;

    for( below =
           inclusive && isMultipleOf( value, multipleOf)
           ? value
           : value.divide( multipleOf, 0, UP).subtract( BigDecimal.ONE).multiply( multipleOf);

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

    for( above =
           inclusive && isMultipleOf( value, multipleOf)
           ? value
           : value.divide( multipleOf, 0, DOWN).add( BigDecimal.ONE).multiply( multipleOf);

         Stream.of( above)
           .filter( a -> notMultipleOfs.stream().anyMatch( m -> isMultipleOf( a, m)))
           .findAny()
           .isPresent();

         above = above.add( multipleOf));
           
    return above;
    }

  /**
   * Return the number of values between the given mininim and maximum (inclusive) that satisfy the given (not-)multiple-of constraints.
   */
  private int multiplesBetween( BigDecimal min, BigDecimal max, BigDecimal multipleOf, Set<BigDecimal> notMultipleOfs)
    {
    int count = 0;

    for( BigDecimal nextMultiple = min;
         nextMultiple.compareTo( max) <= 0;
         nextMultiple = nextMultiple.add( multipleOf))
      {
      BigDecimal checkValue = nextMultiple;
      if( notMultipleOfs.stream().allMatch( m -> !isMultipleOf( checkValue, m)))
        {
        count++;
        }
      }

    return count;
    }

  /**
   * Returns the maximum number of non-null values that can satisfy any alternatives for this schema.
   * Returns null if the number of satisfying values is unbounded.
   */
  private Integer getMaxAlternativeValues( Schema<?> schema)
    {
    return
      Optional.ofNullable( getDnf( schema))
      .map( dnf -> dnf.getAlternatives().stream())
      .orElse( Stream.of( schema))
      .map( s -> getMaxValues( s))
      .filter( Objects::nonNull)
      .sorted()
      .findFirst()
      .orElse( null);
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
    return
      Arrays.stream( mediaType.split( "/"))
      .map( part -> {
        return
          toIdentifier(
            part
            .replace( ' ', '.')
            .replace( '*', 'W')
            .replace( '+', 'S')
            .replace( ';', 'P')
            .replace( '=', 'E'));
        })
      .collect( joining( "-"));
    }

  /**
   * Returns input variable tag for the given media type
   */
  private String mediaTypeVarTag( String contentVarTag, String mediaType)
    {
    return contentVarTag.replace( "Content", "") + mediaTypeVarName( mediaType);
    }

  /**
   * Returns the name that identifies a value not in the given set of enumerated values.
   */
  private String stringNotEnumerated( Set<?> enums)
    {
    // In case "Other", etc. is already listed as an enumerated string value.
    return
      Stream.of( "Other", "Not enumerated", "Not listed", "?")
      .filter( other -> enums.stream().noneMatch( e -> String.valueOf(e).equals( other)))
      .findFirst()
      .orElseThrow( () -> new IllegalStateException( "Can't find a name for a non-enumerated value"));
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
   * Returns the "valid length" property for the given schema instance.
   */
  private String validLengthProperty( String instanceTag)
    {
    return instanceTag + "LengthValid";
    }

  /**
   * Returns the "not empty" property for the given schema instance.
   */
  private String notEmptyProperty( String instanceTag)
    {
    return instanceTag + "NotEmpty";
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
   * Returns the "security requirement defined" property.
   */
  private String secReqProperty()
    {
    return secReqProperty( null);
    }

  /**
   * Returns the "defined" property for the given security requirement.
   */
  private String secReqProperty( String secReqTag)
    {
    return "secRec" + Optional.ofNullable( secReqTag).orElse( "");
    }

  /**
   * Returns the "defined" property for the given security scheme.
   */
  private String secSchemeProperty( String secReqTag, String scheme)
    {
    return secReqProperty( secReqTag) + StringUtils.capitalize( scheme);
    }

  /**
   * Returns the enumerated number values defined by the given schema.
   */
  private Set<BigDecimal> getNumberEnum( Schema<?> schema)
    {
    return
      schema instanceof NumberSchema
      ? asOrderedSet( ((NumberSchema) schema).getEnum())
      : getNumberEnum( schema.getEnum());
    }

  /**
   * Returns the enumerated number values defined by the given list.
   */
  private Set<BigDecimal> getNumberEnum( Iterable<?> values)
    {
    return
      asOrderedSet( values)
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
      .collect( toOrderedSet());
    }

  /**
   * Returns the enumerated boolean values defined by the given schema.
   */
  private Set<Boolean> getBooleanEnum( Schema<?> schema)
    {
    return
      schema instanceof BooleanSchema
      ? asOrderedSet( ((BooleanSchema) schema).getEnum())
      : getBooleanEnum( schema.getEnum());
    }

  /**
   * Returns the enumerated boolean values defined by the given list.
   */
  private Set<Boolean> getBooleanEnum( Iterable<?> values)
    {
    return
      asOrderedSet( values)
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
      .collect( toOrderedSet());
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
  private Set<FormattedString> getFormattedStrings( String description, String format, Iterable<?> value)
    {
    try
      {
      return asOrderedSet( FormattedString.of( format, value));
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
   * Returns a new DNF that generates a simpler but equivalent input model.
   */
  private static Dnf simplified( Dnf dnf)
    {
    return
      Optional.of( dnf)
      .map( InputModeller::removeFalse)
      .map( InputModeller::simplifyTypeChecks)
      .map( InputModeller::simplifyNullChecks)
      .get();
    }

  /**
   * Returns a DNF that eliminates superfluous "false" alternatives.
   */
  private static Dnf removeFalse( Dnf dnf)
    {
    return Dnf.of( dnf.getAlternatives().stream().filter( s -> !isFalse( s)));
    }

  /**
   * Returns a DNF that eliminates redundant type checks.
   */
  private static Dnf simplifyTypeChecks( Dnf dnf)
    {
    Set<String> types = dnf.getTypes();
    if( types.size() > 1)
      {
      // Null type alternatives exist?
      Schema<?> typeChecked; 
      if( types.contains( "null"))
        {
        // Yes, an alternative exists for any type -- no alternative requires a type check
        typeChecked = null;
        }
      else
        {
        // No, consolidate type check on a single alternative
        List<String> typeList = types.stream().collect( toList());
        typeChecked = dnf.getAlternatives( typeList.get(0)).stream().findFirst().get();
        setNotTypes( typeChecked, typeList.subList( 1, typeList.size()));
        }

      // Designate no type check for all other alternatives
      dnf.getAlternatives().stream()
        .filter( s -> s != typeChecked)
        .forEach( s -> setTypeChecked( s, false));
      }
    
    return dnf;
    }

  /**
   * Returns a DNF that eliminates redundant null checks.
   */
  private static Dnf simplifyNullChecks( Dnf dnf)
    {
    // Given the set of nullable assertions made by all alternatives...
    Set<Boolean> nullables =
      dnf.getAlternatives().stream()
      .map( Schema::getNullable)
      .filter( Objects::nonNull)
      .collect( toSet());

    // ... define a single common nullable assertion, with "true" prevailing in certain situations 
    // that produce conflicting alternatives
    boolean nullable = nullables.contains( Boolean.TRUE);

    // Reforming alternatives with a common nullable assertion...
    Dnf simplified = Dnf.of( dnf.getAlternatives().stream().map( s -> { s.setNullable( nullable); return s; }));

    // ... designate null checking for only one of them.
    List<Schema<?>> simplifiedAlternatives = simplified.getAlternatives();
    IntStream.range( 0, simplifiedAlternatives.size())
      .forEach( i -> setNullChecked( simplifiedAlternatives.get(i), i == 0));

    return simplified;
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
