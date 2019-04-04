/////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.ICondition;
import org.cornutum.tcases.util.ListBuilder;
import static org.cornutum.tcases.DefUtils.toIdentifier;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
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
import io.swagger.v3.oas.models.servers.Server;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeSet;
import java.util.function.Function;
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
 * Defines methods for converting between OpenAPI models and Tcases models.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public final class TcasesOpenApi
  {
  /**
   * Creates a new TcasesOpenApi instance.
   */
  private TcasesOpenApi()
    {
    // Static methods only
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI specification. Returns null if the given spec defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( OpenAPI api)
    {
    Info info = expectedValueOf( api.getInfo(), "API info");

    SystemInputDef inputDef =
      SystemInputDefBuilder.with( toIdentifier( expectedValueOf( StringUtils.trimToNull( info.getTitle()), "API title")))
      .has( "version", info.getVersion())
      .hasIf( "server", membersOf( api.getServers()).findFirst().map( Server::getUrl))
      .functions( entriesOf( api.getPaths()).flatMap( path -> pathFunctionDefs( api, path.getKey(), path.getValue())))
      .build();

    return
      inputDef.getFunctionInputDefs().hasNext()
      ? inputDef
      : null;
    }

  /**
   * Returns a {@link FunctionInputDef function input definition} for each of the API operations for the given path.
   */
  private static Stream<FunctionInputDef> pathFunctionDefs( OpenAPI api, String path, PathItem pathItem)
    {
    return
      Stream.of(
        opFunctionDef( api, path, pathItem, "GET", pathItem.getGet()),
        opFunctionDef( api, path, pathItem, "PUT", pathItem.getPut()),
        opFunctionDef( api, path, pathItem, "POST", pathItem.getPost()),
        opFunctionDef( api, path, pathItem, "DELETE", pathItem.getDelete()),
        opFunctionDef( api, path, pathItem, "OPTIONS", pathItem.getOptions()),
        opFunctionDef( api, path, pathItem, "HEAD", pathItem.getHead()),
        opFunctionDef( api, path, pathItem, "PATCH", pathItem.getPatch()),
        opFunctionDef( api, path, pathItem, "TRACE", pathItem.getTrace()))

      // Skip if operation not defined
      .filter( Objects::nonNull)

      // Skip if operation has no inputs to model
      .filter( functionDef -> functionDef.getVarDefs().hasNext());
    }

  /**
   * Returns the {@link FunctionInputDef function input definition} for the given API operation.
   */
  private static FunctionInputDef opFunctionDef( OpenAPI api, String path, PathItem pathItem, String opName, Operation op)
    {
    try
      {
      return
        op == null?
        null :

        FunctionInputDefBuilder.with( String.format( "%s_%s", opName, functionPathName( path)))
        .hasIf( "server", membersOf( pathItem.getServers()).findFirst().map( Server::getUrl))
        .hasIf( "server", membersOf( op.getServers()).findFirst().map( Server::getUrl))
        .vars( opParameters( pathItem, op).map( p -> parameterVarDef( api, resolveParameter( api, p))))
        .vars( iterableOf( requestBodyVarDef( api, op.getRequestBody())))
        .build();
      }
    catch( Exception e)
      {
      throw new OpenApiException( String.format( "Error processing %s %s", opName, path), e);
      }
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given request body.
   */
  private static Optional<IVarDef> requestBodyVarDef( OpenAPI api, RequestBody body)
    {
    try
      {
      return
        Optional.ofNullable( body)
        .map( b -> resolveRequestBody( api, b))
        .map( b ->
          {
          String instanceVarName = "Body";
          
          return
            VarSetBuilder.with( instanceVarName)
            .type( "request")
            .members(
              instanceDefinedVar( instanceVarName, Boolean.TRUE.equals( b.getRequired())),
              mediaTypeVar( instanceVarName, b))
            .members(
              mediaTypeContentVars( api, b))
            .build();
          });
      }
    catch( Exception e)
      {
      throw new OpenApiException( "Error processing request body", e);
      }
    }

  /**
   * Returns the {@link IVarDef input variable definition} for request body media types.
   */
  private static IVarDef mediaTypeVar( String bodyVarTag, RequestBody body)
    {
    return
      VarDefBuilder.with( "Media-Type")
      .when( instanceDefinedCondition( bodyVarTag))
      .values(
        mediaTypeContentDefs( body)
        .entrySet().stream()
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
   * Returns the {@link IVarDef input variable definitions} for request body content.
   */
  private static Stream<IVarDef> mediaTypeContentVars( OpenAPI api, RequestBody body)
    {
    return
      mediaTypeContentDefs( body)
      .entrySet().stream()
      .map(
        contentDef ->
        {
        String mediaType = contentDef.getKey();
        try
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
          }
        catch( Exception e)
          {
          throw new OpenApiException( String.format( "Error processing media type=%s", mediaType), e);
          }
        });
    }

  /**
   * Returns the map of media type content definitions for the request body.
   */
  private static Map<String,MediaType> mediaTypeContentDefs( RequestBody body)
    {
    return expectedValueOf( body.getContent(), "Request body content");
    }

  /**
   * Returns the {@link IVarDef input variable definition} for the given parameter.
   */
  private static IVarDef parameterVarDef( OpenAPI api, Parameter parameter)
    {
    try
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
      }
    catch( Exception e)
      {
      throw new OpenApiException( String.format( "Error processing parameter=%s", parameter.getName()), e);
      }
    }

  /**
   * Returns the schema for the given parameter.
   */
  private static Schema<?> parameterSchema( OpenAPI api, Parameter parameter)
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
  private static Parameter.StyleEnum parameterStyle( Parameter parameter)
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
  private static Boolean parameterExplode( Boolean explode, String parameterType, Parameter.StyleEnum parameterStyle)
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
  private static IVarDef parameterDefinedVar( String parameterVarTag, String parameterType, Parameter parameter)
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
   * Returns an {@link IVarDef input variable} to represent if the given instance is defined.
   */
  private static VarDef instanceDefinedVar( String instanceVarTag, boolean required, String... propertiesWhenDefined)
    {
    return
      VarDefBuilder.with( "Defined")
      .values(
        VarValueDefBuilder.with( "Yes")
        .properties( instanceDefinedProperty( instanceVarTag))
        .properties( propertiesWhenDefined)
        .build(),

        VarValueDefBuilder.with( "No")
        .type( required? VarValueDef.Type.FAILURE : VarValueDef.Type.VALID)
        .build())
      .build();
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private static Stream<IVarDef> instanceSchemaVars( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    return instanceSchemaVars( api, instanceVarTag, true, instanceSchema);      
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the schema for the given instance.
   */
  private static Stream<IVarDef> instanceSchemaVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    ComposedSchema composedSchema = instanceSchema instanceof ComposedSchema? (ComposedSchema) instanceSchema : null;
    String type = composedSchema == null? expectedValueOf( instanceSchema.getType(), "type") : null;

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
  private static Stream<IVarDef> instanceArrayVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
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
          instanceArraySizeVar( api, instanceVarTag, arraySchema),
          instanceItemVar( api, instanceVarTag, itemSchema))
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the size of an array instance.
   */
  private static IVarDef instanceArraySizeVar( OpenAPI api, String instanceVarTag, ArraySchema arraySchema)
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
          else if( sizeValue == 0)
            {
            sizeBuilder.type( VarValueDef.Type.ONCE);
            }
          else
            {
            sizeBuilder
              .properties( arrayItemsProperty( instanceVarTag))
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
  private static IVarDef instanceItemVar( OpenAPI api, String instanceVarTag, Schema<?> itemSchema)
    {
    try
      {
      boolean uniqueItems = Boolean.TRUE.equals( itemSchema.getUniqueItems());

      return
        VarSetBuilder.with( "Contains")
        .when( has( arrayItemsProperty( instanceVarTag)))

        .members( instanceSchemaVars( api, arrayItemsProperty( instanceVarTag), false, itemSchema))

        .members(
          VarDefBuilder.with( "Unique")
          .when( has( arrayItemsManyProperty( instanceVarTag)))
          .values(
            VarValueDefBuilder.with( "Yes").build(),
            VarValueDefBuilder.with( "No").type( uniqueItems? VarValueDef.Type.FAILURE: VarValueDef.Type.VALID).build())
          .build())

        .build();
      }
    catch( Exception e)
      {
      throw new OpenApiException( "Error processing array item schema", e);
      }
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given boolean instance.
   */
  private static Stream<IVarDef> instanceBooleanVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        booleanValueVar( api, instanceVarTag, instanceSchema));
    }    

  /**
   * Returns the {@link IVarDef input variables} defined by the given integer instance.
   */
  private static Stream<IVarDef> instanceIntegerVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        integerValueVar( api, instanceVarTag, instanceSchema));
    }   

  /**
   * Returns the {@link IVarDef input variables} defined by the given number instance.
   */
  private static Stream<IVarDef> instanceNumberVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        numberValueVar( api, instanceVarTag, instanceSchema));
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given object instance.
   */
  private static Stream<IVarDef> instanceObjectVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        objectValueVar( api, instanceVarTag, instanceSchema));
    }

  /**
   * Returns the {@link IVarDef input variables} defined by the given string instance.
   */
  private static Stream<IVarDef> instanceStringVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
    {
    return
      Stream.of(
        instanceTypeVar( api, instanceVarTag, instanceOptional, instanceSchema),
        stringValueVar( api, instanceVarTag, instanceSchema));
    }   

  /**
   * Returns the {@link IVarDef input variables} defined by the given composed schema.
   */
  private static Stream<IVarDef> composedSchemaVars( OpenAPI api, String instanceVarTag, boolean instanceOptional, ComposedSchema composedSchema)
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
  private static IVarDef allOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, List<Schema> memberSchemas)
    {
    String exprType = "AllOf";
    
    return
      VarSetBuilder.with( exprType)
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .members(
        IntStream.range( 0, memberSchemas.size())
        .mapToObj(
          i ->
          {
          try
            {
            String exprVarTag = instanceVarTag + exprType + i;
            return
              VarSetBuilder.with( String.valueOf(i))
              .members( instanceSchemaVars( api, exprVarTag, false, resolveSchema( api, memberSchemas.get(i))))
              .build();
            }
          catch( Exception e)
            {
            throw new OpenApiException( String.format( "Error processing 'allOf', member schema %s", i), e);
            }
          }))
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} defined by the given "anyOf" schema.
   */
  @SuppressWarnings("rawtypes")
  private static IVarDef anyOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, List<Schema> memberSchemas)
    {
    return memberChoiceVar( api, "anyOf", instanceVarTag, instanceOptional, memberSchemas);
    } 

  /**
   * Returns the {@link IVarDef input variable} defined by the given "oneOf" schema.
   */
  private static IVarDef oneOfVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, @SuppressWarnings("rawtypes") List<Schema> memberSchemas)
    {
    return memberChoiceVar( api, "oneOf", instanceVarTag, instanceOptional, memberSchemas);
    }

  /**
   * Returns the {@link IVarDef input variable} defined by a schema matching one of the given member schemas.
   */
  @SuppressWarnings("rawtypes")
  private static IVarDef memberChoiceVar( OpenAPI api, String instanceSchemaType, String instanceVarTag, boolean instanceOptional, List<Schema> memberSchemas)
    {
    String exprType = StringUtils.capitalize( instanceSchemaType);
    
    return
      VarSetBuilder.with( exprType)
      .when( instanceDefinedCondition( instanceVarTag, instanceOptional))
      .members(
        VarDefBuilder.with( "Matches")
        .values(
          IntStream.range( 0, memberSchemas.size())
          .mapToObj(
            i ->
            {
            String exprVarTag = instanceVarTag + exprType + i;
            return
              VarValueDefBuilder.with( i)
              .properties( instanceDefinedProperty( exprVarTag))
              .build();
            }))
        .build())
      .members(
        IntStream.range( 0, memberSchemas.size())
        .mapToObj(
          i ->
          {
          try
            {
            String exprVarTag = instanceVarTag + exprType + i;
            return
              VarSetBuilder.with( String.valueOf(i))
              .when( has( instanceDefinedProperty( exprVarTag)))
              .members( instanceSchemaVars( api, exprVarTag, false, resolveSchema( api, memberSchemas.get(i))))
              .build();
            }
          catch( Exception e)
            {
            throw new OpenApiException( String.format( "Error processing '%s', member schema %s", instanceSchemaType, i), e);
            }
          }))
      .build();
    }

  /**
   * Returns the {@link IVarDef input variable} representing the type of a instance.
   */
  private static IVarDef instanceTypeVar( OpenAPI api, String instanceVarTag, boolean instanceOptional, Schema<?> instanceSchema)
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
  private static IVarDef integerValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
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
  private static IVarDef numberValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
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
  private static IVarDef objectValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    VarSetBuilder value =
      VarSetBuilder.with( "Value")
      .when( has( instanceValueProperty( instanceVarTag)));

    objectPropertyCountVar( api, instanceVarTag, instanceSchema)
      .ifPresent( var -> value.members( var));
      
    objectPropertiesVar( api, instanceVarTag, instanceSchema)
      .ifPresent( var -> value.members( var));

    objectAdditionalVar( api, instanceVarTag, instanceSchema)
      .ifPresent( var -> value.members( var));

    return value.build();
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the number of properties for an object instance.
   */
  private static Optional<IVarDef> objectPropertyCountVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
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
      .filter( min -> min > requiredCount && min < totalCount)
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
        countValues
          .add(
            VarValueDefBuilder.with( String.format( ">= %s", minProperties))
            .when( notLessThan( objectPropertiesProperty( instanceVarTag), minProperties))
            .build())
          .add(
            VarValueDefBuilder.with( String.format( "< %s", minProperties))
            .when( lessThan( objectPropertiesProperty( instanceVarTag), minProperties)) 
            .type( VarValueDef.Type.FAILURE)
            .build());
        }
      }

    // No, but are all properties optional?
    else if( requiredCount == 0)
      {
      // Yes, ensure coverage of "no properties" case.
      boolean hasAny = totalCount > 0 || hasAdditional;
      countValues
        .add(
          VarValueDefBuilder.with( 0)
          .when( hasAny? not( objectPropertiesProperty( instanceVarTag)) : null) 
          .type( hasAny? VarValueDef.Type.ONCE : VarValueDef.Type.VALID)
          .build())
        .add(
          VarValueDefBuilder.with( "> 0")
          .type( hasAny? VarValueDef.Type.VALID : VarValueDef.Type.FAILURE)
          .when( hasAny? has( objectPropertiesProperty( instanceVarTag)) : null) 
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
  private static Optional<IVarDef> objectPropertiesVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    Map<String,Schema> propertyDefs = Optional.ofNullable( instanceSchema.getProperties()).orElse( emptyMap());
    List<String> requiredProperties = Optional.ofNullable( instanceSchema.getRequired()).orElse( emptyList());
    
    List<IVarDef> members =
      propertyDefs.entrySet().stream()

      .map(
        propertyDef ->
        objectPropertyVar(
          api,
          instanceVarTag,
          propertyDef.getKey(),
          requiredProperties.contains( propertyDef.getKey()),
          resolveSchema( api, propertyDef.getValue())))

      .collect( toList());

    return
      members.isEmpty()?
      Optional.empty() :

      Optional.of(
        VarSetBuilder.with( "Properties")
        .members( members)
        .build());
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for an object instance property.
   */
  private static IVarDef objectPropertyVar( OpenAPI api, String instanceVarTag, String propertyName, boolean required, Schema<?> propertySchema)
    {
    try
      {
      String propertyVarName = toIdentifier( propertyName);
      String propertyVarTag = instanceVarTag + StringUtils.capitalize( propertyVarName);
      
      return
        VarSetBuilder.with( propertyVarName)
        .members( instanceDefinedVar( propertyVarTag, required, objectPropertiesProperty( instanceVarTag)))
        .members( instanceSchemaVars( api, propertyVarTag, propertySchema))
        .build();
      }
    catch( Exception e)
      {
      throw new OpenApiException( String.format( "Error processing property=%s", propertyName), e);
      }
    }
  
  /**
   * Returns the {@link IVarDef input variable} representing the additional properties of an object instance.
   */
  private static Optional<IVarDef> objectAdditionalVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
    {
    IVarDef varDef = null;
    if( instanceSchema.getAdditionalProperties() != null)
      {
      Schema<?> propertySchema =
        instanceSchema.getAdditionalProperties().getClass().equals( Boolean.class)
        ? null
        : (Schema<?>)instanceSchema.getAdditionalProperties();

      boolean allowed =
        propertySchema != null
        || Boolean.TRUE.equals((Boolean) instanceSchema.getAdditionalProperties());
        
      varDef =
        propertySchema == null ?

        VarDefBuilder.with( "Additional")
        .values(
          VarValueDefBuilder.with( "Yes")
          .type( allowed? VarValueDef.Type.VALID : VarValueDef.Type.FAILURE)
          .properties( Optional.ofNullable( allowed? objectPropertiesProperty( instanceVarTag) : null))
          .build(),

          VarValueDefBuilder.with( "No")
          .build())
        .build() :

        objectPropertyVar( api, instanceVarTag, "Additional", false, propertySchema);
      }

    return Optional.ofNullable( varDef);
    }

  /**
   * Returns the {@link IVarDef input variable} representing the values for a string instance.
   */
  private static IVarDef stringValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
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
  private static IVarDef booleanValueVar( OpenAPI api, String instanceVarTag, Schema<?> instanceSchema)
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
   * Returns the consolidated set of parameters for the given API operation.
   */
  private static Stream<Parameter> opParameters( PathItem pathItem, Operation op)
    {
    return
      Stream.concat( membersOf( pathItem.getParameters()), membersOf( op.getParameters()))
      .collect( toMap( Parameter::getName, Function.identity(), (pathParam, opParam) -> opParam, () -> new LinkedHashMap<String,Parameter>()))
      .values()
      .stream();
    }

  /**
   * Returns the component of a function name that represents the given API request path.
   */
  private static String functionPathName( String pathName)
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
  private static String mediaTypeVarName( String mediaType)
    {
    return functionPathName( mediaType);
    }

  /**
   * Returns "defined" condition for the given instance.
   */
  private static ICondition instanceDefinedCondition( String instanceTag, boolean instanceOptional)
    {
    return
      instanceOptional
      ? has( instanceDefinedProperty( instanceTag))
      : null;
    }

  /**
   * Returns "defined" condition for the given instance.
   */
  private static ICondition instanceDefinedCondition( String instanceTag)
    {
    return instanceDefinedCondition( instanceTag, true);
    }

  /**
   * Returns the "defined" property for the given instance.
   */
  private static String instanceDefinedProperty( String instanceTag)
    {
    return instanceTag;
    }

  /**
   * Returns the "has value" property for the given instance.
   */
  private static String instanceValueProperty( String instanceTag)
    {
    return instanceTag + "Value";
    }

  /**
   * Returns the "has items" property for the given array instance.
   */
  private static String arrayItemsProperty( String instanceTag)
    {
    return instanceTag + "Items";
    }

  /**
   * Returns the "has many items" property for the given array instance.
   */
  private static String arrayItemsManyProperty( String instanceTag)
    {
    return arrayItemsProperty( instanceTag) + "Many";
    }

  /**
   * Returns the "has properties" property for the given object instance.
   */
  private static String objectPropertiesProperty( String instanceTag)
    {
    return instanceTag + "Properties";
    }

  /**
   * Returns the given value if non-null. Otherwise, throws an OpenApiException.
   */
  private static <T> T expectedValueOf( T value, String description, Object... descriptionArgs)
    {
    if( value == null)
      {
      throw new OpenApiException( String.format( description, descriptionArgs) + " is not defined");
      }

    return value;
    }

  /**
   * If the given parameter is defined by a reference, returns the referenced parameter. Otherwise, returns the given parameter.
   */
  private static Parameter resolveParameter( OpenAPI api, Parameter parameter)
    {
    return componentParameterRef( api, parameter.get$ref()).orElse( parameter);
    }

  /**
   * If the given schema is defined by a reference, returns the referenced schema. Otherwise, returns the given schema.
   */
  private static Schema<?> resolveSchema( OpenAPI api, Schema<?> schema)
    {
    return componentSchemaRef( api, schema.get$ref()).orElse( schema);
    }

  /**
   * If the given request body is defined by a reference, returns the referenced requestBody. Otherwise, returns the given request body.
   */
  private static RequestBody resolveRequestBody( OpenAPI api, RequestBody requestBody)
    {
    return componentRequestBodyRef( api, requestBody.get$ref()).orElse( requestBody);
    }

  /**
   * If the given response is defined by a reference, returns the referenced response. Otherwise, returns the given response.
   */
  private static ApiResponse resolveResponse( OpenAPI api, ApiResponse response)
    {
    return componentResponseRef( api, response.get$ref()).orElse( response);
    }

  /**
   * When the given reference is non-null, returns the component parameter referenced.
   */
  private static Optional<Parameter> componentParameterRef( OpenAPI api, String reference)
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
  private static Optional<Schema> componentSchemaRef( OpenAPI api, String reference)
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
  private static Optional<RequestBody> componentRequestBodyRef( OpenAPI api, String reference)
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
  private static Optional<ApiResponse> componentResponseRef( OpenAPI api, String reference)
    {
    return
      Optional.ofNullable( reference)

      .map( ref -> componentName( componentsResponsesRef_, ref))
      .filter( Objects::nonNull)

      .map( name -> expectedValueOf( expectedValueOf( api.getComponents(), "Components").getResponses(), "Component responses").get( name))
      .filter( Objects::nonNull);
    }

  /**
   * Returns the name of the given component reference.
   */
  private static String componentName( String refType, String ref)
    {
    return ref.startsWith( refType)? ref.substring( refType.length()) : null;
    }

  private static final String componentsParametersRef_ = "#/components/parameters/";
  private static final String componentsRequestBodiesRef_ = "#/components/requestBodies/";
  private static final String componentsResponsesRef_ = "#/components/responses/";
  private static final String componentsSchemasRef_ = "#/components/schemas/";
}
