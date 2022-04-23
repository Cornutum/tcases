//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver.io;

import org.cornutum.tcases.openapi.resolver.*;
import org.cornutum.tcases.openapi.resolver.ParamDef.Location;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.JsonValue.ValueType;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Converts between a {@link RequestCase} and its corresponding {@link JsonObject}.
 */
public final class RequestCaseJson
  {
  /**
   * Creates a new RequestCaseJson instance.
   */
  private RequestCaseJson()
    {
    // Static methods only
    }

  /**
   * Returns the JSON array that represents the given request cases.
   */
  public static JsonArray toJson( RequestTestDef requestTestDef)
    {
    JsonArrayBuilder builder = Json.createArrayBuilder();
    for( RequestCase requestCase : requestTestDef.getRequestCases())
      {
      builder.add( toJson( requestCase));
      }
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given request case.
   */
  private static JsonObject toJson( RequestCase requestCase)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( ID, requestCase.getId());
    Optional.ofNullable( requestCase.getName()).ifPresent( name -> builder.add( NAME, name));
    Optional.ofNullable( requestCase.getServer()).ifPresent( server -> builder.add( SERVER, server.toString()));
    Optional.ofNullable( requestCase.getVersion()).ifPresent( version -> builder.add( VERSION, version));
    Optional.ofNullable( requestCase.getApi()).ifPresent( api -> builder.add( API, api));
    Optional.ofNullable( requestCase.getPath()).ifPresent( path -> builder.add( PATH, path));
    Optional.ofNullable( requestCase.getOperation()).ifPresent( op -> builder.add( OPERATION, op));

    JsonArrayBuilder paramBuilder = Json.createArrayBuilder();
    toStream( requestCase.getParams()).forEach( paramData -> paramBuilder.add( toJson( paramData)));
    JsonArray params = paramBuilder.build();
    if( !params.isEmpty())
      {
      builder.add( PARAMETERS, params);
      }
    
    Optional.ofNullable( requestCase.getBody()).ifPresent( body -> builder.add( BODY, toJson( body)));

    JsonArrayBuilder authBuilder = Json.createArrayBuilder();
    toStream( requestCase.getAuthDefs()).forEach( authDef -> authBuilder.add( AuthDefJson.toJson( authDef)));
    JsonArray authDefs = authBuilder.build();
    if( !authDefs.isEmpty())
      {
      builder.add( AUTH, authDefs);
      }
    
    Optional.ofNullable( requestCase.getInvalidInput()).ifPresent( invalidInput -> builder.add( INVALID_INPUT, invalidInput));
    if( requestCase.isAuthFailure())
      {
      builder.add( AUTH_FAILURE, true);
      }
    
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given parameter data.
   */
  private static JsonObject toJson( ParamData paramData)
    {
    JsonObjectBuilder builder = 
      Json.createObjectBuilder()
      .add( NAME, paramData.getName())
      .add( IN, paramData.getLocation().toString().toLowerCase())
      .add( STYLE, paramData.getStyle())
      .add( EXPLODE, paramData.isExploded()) ;

    return addMessageData( builder, paramData).build();
    }

  /**
   * Returns the JSON object that represents the given message data.
   */
  private static JsonObject toJson( MessageData messageData)
    {
    return addMessageData( Json.createObjectBuilder(), messageData).build();
    }

  /**
   * Adds message data properties to the given JSON object builder.
   */
  private static JsonObjectBuilder addMessageData( JsonObjectBuilder builder, MessageData messageData)
    {
    Optional.ofNullable( messageData.getMediaType()).ifPresent( mediaType -> builder.add( MEDIA_TYPE, mediaType));

    JsonValue dataValue =
      Optional.ofNullable( messageData.getValue())
      .map( v -> (JsonValue) toJson( v))
      .orElse( JsonValue.NULL);
    builder.add( DATA, dataValue);
    
    builder.add( VALID, messageData.isValid());

    if( !messageData.getEncodings().isEmpty())
      {
      JsonObjectBuilder encodings = Json.createObjectBuilder();
      messageData.getEncodings().forEach( (property,encoding) -> encodings.add( property, toJson( messageData.getMediaType(), encoding)));
      builder.add( ENCODINGS, encodings.build());
      }
    
    return builder;
    }

  /**
   * Adds a form part encoding to the given JSON object builder.
   */
  private static JsonObject toJson( String mediaType, EncodingData encoding)
    {
    JsonObject json;

    if( "application/x-www-form-urlencoded".equals( mediaType))
      {
      json = 
        Json.createObjectBuilder()
        .add( STYLE, encoding.getStyle())
        .add( EXPLODE, encoding.isExploded())
        .build();      
      }
    else if( "multipart/form-data".equals( mediaType))
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      builder.add( CONTENT_TYPE, encoding.getContentType());

      if( !encoding.getHeaders().isEmpty())
        {
        JsonArrayBuilder headersBuilder = Json.createArrayBuilder();
        encoding.getHeaders().forEach( header -> headersBuilder.add( toJson( header)));
        builder.add( HEADERS, headersBuilder.build());
        }

      json = builder.build();
      }
    else
      {
      throw new RequestCaseException( String.format( "Part encodings not defined for mediaType=%s", mediaType));
      }

    return json;
    }

  /**
   * Returns the JSON object that represents the given header data.
   */
  private static JsonObject toJson( HeaderData headerData)
    {
    JsonObjectBuilder builder = 
      Json.createObjectBuilder()
      .add( NAME, headerData.getName())
      .add( EXPLODE, headerData.isExploded()) ;

    return addMessageData( builder, headerData).build();
    }

  /**
   * Returns the JSON object that represents the given data value.
   */
  private static JsonObject toJson( DataValue<?> dataValue)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( TYPE, String.valueOf( dataValue.getType()).toLowerCase());
    builder.add( VALUE, DataValueJson.toJson( dataValue));
    Optional.ofNullable( dataValue.getFormat()).ifPresent( format -> builder.add( FORMAT, format));
    
    return builder.build();
    }

  /**
   * Returns the RequestCase instances represented by the given JSON array.
   */
  public static List<RequestCase> asRequestCases( JsonArray json)
    {
    RequestCaseContext context = new RequestCaseContext();
    return
      json.getValuesAs( JsonObject.class)
      .stream()
      .map( rcJson -> asRequestCase( context, rcJson))
      .collect( toList());
    }

  /**
   * Returns the RequestCase represented by the given JSON object.
   */
  private static RequestCase asRequestCase( RequestCaseContext context, JsonObject json)
    {
    return
      context.resultFor( String.valueOf( json.get( ID)), () -> {
        RequestCase requestCase = new RequestCase( json.getInt( ID));

        requestCase.setName( context.resultFor( NAME, () -> json.getString( NAME, null)));
        requestCase.setServer( context.resultFor( SERVER, () -> json.getString( SERVER, null)));
        requestCase.setVersion( context.resultFor( VERSION, () -> json.getString( VERSION, null)));
        requestCase.setApi( context.resultFor( API, () -> json.getString( API, null)));
        requestCase.setPath( context.resultFor( PATH, () -> json.getString( PATH, null)));
        requestCase.setOperation( context.resultFor( OPERATION, () -> json.getString( OPERATION, null)));
        requestCase.setInvalidInput( context.resultFor( INVALID_INPUT, () -> json.getString( INVALID_INPUT, null)));

        context.doFor( PARAMETERS, () -> {
          Optional.ofNullable( json.getJsonArray( PARAMETERS))
            .map( array -> array.getValuesAs( JsonObject.class))
            .ifPresent( params -> params.stream().forEach( param -> requestCase.addParam( asParamData( context, param))));
          });

        context.doFor( BODY, () -> {
          Optional.ofNullable( json.getJsonObject( BODY))
            .ifPresent( body -> requestCase.setBody( asMessageData( context, body)));
          });

        context.doFor( AUTH, () -> {
          Optional.ofNullable( json.getJsonArray( AUTH))
            .map( array -> array.getValuesAs( JsonObject.class))
            .ifPresent( authDefs -> authDefs.stream().forEach( authDef -> requestCase.addAuthDef( asAuthDef( context, authDef))));
          });
        requestCase.setAuthFailure( context.resultFor( AUTH_FAILURE, () -> json.getBoolean( AUTH_FAILURE, false)));

        return requestCase;
        });
    }

  /**
   * Returns the ParamData represented by the given JSON object.
   */
  private static ParamData asParamData( RequestCaseContext context, JsonObject json)
    {
    JsonValue name = json.get( NAME);
    return
      context.resultFor( String.valueOf( name), () -> {
        ParamData paramData = new ParamData( json.getString( NAME), asMessageData( context, json));

        paramData.setLocation( context.resultFor( IN, () -> Location.valueOf( json.getString( IN).toUpperCase())));
        paramData.setStyle( context.resultFor( STYLE, () -> json.getString( STYLE)));
        paramData.setExploded( context.resultFor( EXPLODE, () -> json.getBoolean( EXPLODE, false)));
        
        return paramData;
        });
    }

  /**
   * Returns the MessageData represented by the given JSON object.
   */
  private static MessageData asMessageData( RequestCaseContext context, JsonObject json)
    {
    String mediaType = context.resultFor( MEDIA_TYPE, () -> json.getString( MEDIA_TYPE, null));
    boolean valid = context.resultFor( VALID, () -> json.getBoolean( VALID));
    DataValue<?> value = context.resultFor( DATA, () -> Optional.ofNullable( json.get( DATA)).map( v -> asDataValue( context, v)).orElse( null));
    return new MessageData( value, mediaType, valid);
    }

  /**
   * Returns the DataValue represented by the given JSON object.
   */
  private static DataValue<?> asDataValue( RequestCaseContext context, JsonValue json)
    {
    DataValue<?> data;
    ValueType jsonType = json.getValueType();
    if( jsonType == ValueType.NULL)
      {
      data = null;
      }
    else if( jsonType == ValueType.OBJECT)
      {
      JsonObject object = (JsonObject) json;
      String type = context.resultFor( TYPE, () -> object.getString( TYPE));
      String format = context.resultFor( FORMAT, () -> object.getString( FORMAT, null));

      data =
        context.resultFor( VALUE, () -> {
          return
            type.equals( "array")?
            asArrayValue( context, object.getJsonArray( VALUE)) :

            type.equals( "boolean")?
            new BooleanValue( object.getBoolean( VALUE)) :
        
            type.equals( "integer")?
            asIntegerValue( context, object.getJsonNumber( VALUE), format) :

            type.equals( "null")?
            new NullValue() :

            type.equals( "number")?
            new DecimalValue( object.getJsonNumber( VALUE).bigDecimalValue(), format) :
        
            type.equals( "object")?
            asObjectValue( context, object.getJsonObject( VALUE)) :

            asStringValue( context, object.getString( VALUE), format);
          });
      }
    else
      {
      throw new RequestCaseException( String.format( "Invalid value type=%s -- must be either \"null\" or \"object\"", jsonType));
      }
    
    return data;
    }

  /**
   * Returns the DataValue represented by the given array object.
   */
  @SuppressWarnings("unchecked")
  private static ArrayValue<?> asArrayValue( RequestCaseContext context, JsonArray json)
    {
    return
      new ArrayValue<Object>(
        IntStream.range( 0, json.size())
        .mapToObj( i -> context.resultFor( String.valueOf( i), () -> (DataValue<Object>) asDataValue( context, json.get(i))))
        .collect( toList()));
    }

  /**
   * Returns the DataValue represented by the given number object.
   */
  private static DataValue<?> asIntegerValue( RequestCaseContext context, JsonNumber json, String format)
    {
    return
      "int64".equals( format)
      ? new LongValue( json.longValueExact())
      : new IntegerValue( json.intValueExact());
    }

  /**
   * Returns the ObjectValue represented by the given JSON object.
   */
  private static ObjectValue asObjectValue( RequestCaseContext context, JsonObject json)
    {
    return
      new ObjectValue(
        json.keySet().stream()
        .collect(
          toMap(
            key -> key,
            key -> context.resultFor( key, () -> asDataValue( context, json.get( key))),
            (v1, v2) -> { throw new IllegalStateException( "Duplicated object property name"); },
            LinkedHashMap::new)));
    }

  /**
   * Returns the DataValue represented by the given string object.
   */
  private static DataValue<?> asStringValue( RequestCaseContext context, String value, String format)
    {
    return
      "date".equals( format)?
      new DateValue( value) :

      "date-time".equals( format)?
      new DateTimeValue( value) :

      "uuid".equals( format)?
      new UuidValue( value) :

      "binary".equals( format)?
      new BinaryValue( Base64Domain.decoded( value)) :

      "email".equals( format)?
      new EmailValue( value) :

      new StringValue( value, format);
    }

  /**
   * Returns the AuthDef represented by the given JSON object.
   */
  private static AuthDef asAuthDef( RequestCaseContext context, JsonObject json)
    {
    AuthDef authDef;
    
    String type = json.getString( TYPE);
    if( "apiKey".equals( type))
      {
      authDef =
        new ApiKeyDef(
          Location.valueOf( json.getString( LOCATION).toUpperCase()),
          json.getString( NAME));
      }
    else if( "http".equals( type))
      {
      String scheme = json.getString( SCHEME);
      if( "basic".equals( scheme))
        {
        authDef = new HttpBasicDef();
        }
      else if( "bearer".equals( scheme))
        {
        authDef = new HttpBearerDef();
        }
      else
        {
        throw new RequestCaseException( String.format( "HTTP authentication scheme=%s is not supported", scheme));
        }
      }
    else
      {
      throw new RequestCaseException( String.format( "Authentication type=%s is not supported", type));
      }
    
    return authDef;
    }

  /**
   * Creates the JSON representation of a {@link DataValue}.
   */
  private static class DataValueJson implements DataValueVisitor
    {
    /**
     * Returns the JSON representation of the given {@link DataValue}.
     */
    public static JsonValue toJson( DataValue<?> value)
      {
      return new DataValueJson( value).toJson();
      }

    /**
     * Creates a new DataValueJson instance.
     */
    private DataValueJson( DataValue<?> value)
      {
      value_ = value;
      }
    
    /**
     * Returns the JSON representation of this {@link DataValue}
     */
    private JsonValue toJson()
      {
      value_.accept( this);
      return json_;
      }

    @Override
    public void visit( ArrayValue<?> data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      data.getValue().stream().forEach( item -> builder.add( RequestCaseJson.toJson( item)));
      json_ = builder.build();
      }

    @Override
    public void visit( BinaryValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( Base64Domain.encoded( data.getValue()));
      json_ = builder.build().get(0);
      }

    @Override
    public void visit( BooleanValue data)
      {
      json_ =
        data.getValue().equals( Boolean.TRUE)
        ? JsonValue.TRUE
        : JsonValue.FALSE;
      }

    @Override
    public void visit( DecimalValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    @Override
    public void visit( IntegerValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    @Override
    public void visit( LongValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    @Override
    public void visit( NullValue data)
      {
      json_ = JsonValue.NULL;
      }

    @Override
    public void visit( ObjectValue data)
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      data.getValue().keySet().stream().forEach( key -> builder.add( key, RequestCaseJson.toJson( data.getValue().get( key))));
      json_ = builder.build();
      }

    @Override
    public void visit( StringValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    private final DataValue<?> value_;
    private JsonValue json_;
    }

  /**
   * Creates the JSON representation of a {@link AuthDef}.
   */
  private static class AuthDefJson implements AuthDefVisitor
    {
    /**
     * Returns the JSON representation of the given {@link AuthDef}.
     */
    public static JsonValue toJson( AuthDef authDef)
      {
      return new AuthDefJson( authDef).toJson();
      }

    /**
     * Creates a new AuthDefJson instance.
     */
    private AuthDefJson( AuthDef authDef)
      {
      authDef_ = authDef;
      }
    
    /**
     * Returns the JSON representation of this {@link AuthDef}
     */
    private JsonValue toJson()
      {
      authDef_.accept( this);
      return json_;
      }

    @Override
    public void visit( ApiKeyDef authDef)
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      builder.add( TYPE, "apiKey");
      builder.add( LOCATION, String.valueOf( authDef.getLocation()).toLowerCase());
      builder.add( NAME, authDef.getName());
      json_ = builder.build();
      }

    @Override
    public void visit( HttpBasicDef authDef)
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      builder.add( TYPE, "http");
      builder.add( SCHEME, "basic");
      json_ = builder.build();
      }

    @Override
    public void visit( HttpBearerDef authDef)
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      builder.add( TYPE, "http");
      builder.add( SCHEME, "bearer");
      json_ = builder.build();
      }

    private final AuthDef authDef_;
    private JsonValue json_;
    }

  private static final String API = "api";
  private static final String AUTH = "auth";
  private static final String AUTH_FAILURE = "authFailure";
  private static final String BODY = "body";
  private static final String CONTENT_TYPE = "contentType";
  private static final String DATA = "data";
  private static final String ENCODINGS = "encodings";
  private static final String EXPLODE = "explode";
  private static final String FORMAT = "format";
  private static final String HEADERS = "headers";
  private static final String ID = "id";
  private static final String IN = "in";
  private static final String INVALID_INPUT = "invalidInput";
  private static final String LOCATION = "location";
  private static final String MEDIA_TYPE = "mediaType";
  private static final String NAME = "name";
  private static final String OPERATION = "operation";
  private static final String PARAMETERS = "parameters";
  private static final String PATH = "path";
  private static final String SCHEME = "scheme";
  private static final String SERVER = "server";
  private static final String STYLE = "style";
  private static final String TYPE = "type";
  private static final String VALID = "valid";
  private static final String VALUE = "value";
  private static final String VERSION = "version";
  }
