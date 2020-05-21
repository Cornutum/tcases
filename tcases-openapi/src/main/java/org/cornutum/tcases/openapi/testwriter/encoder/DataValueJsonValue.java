//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.ArrayValue;
import org.cornutum.tcases.openapi.resolver.Base64Domain;
import org.cornutum.tcases.openapi.resolver.BinaryValue;
import org.cornutum.tcases.openapi.resolver.BooleanValue;
import org.cornutum.tcases.openapi.resolver.DataValue;
import org.cornutum.tcases.openapi.resolver.DataValueVisitor;
import org.cornutum.tcases.openapi.resolver.DecimalValue;
import org.cornutum.tcases.openapi.resolver.IntegerValue;
import org.cornutum.tcases.openapi.resolver.LongValue;
import org.cornutum.tcases.openapi.resolver.NullValue;
import org.cornutum.tcases.openapi.resolver.ObjectValue;
import org.cornutum.tcases.openapi.resolver.StringValue;

import java.util.Optional;
import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;

/**
 * Converts a DataValue into a {@link JsonValue}
 */
public class DataValueJsonValue implements DataValueConverter<JsonValue>
  {
  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  public static JsonValue toJson( DataValue<?> value)
    {
    return new DataValueJsonValue().convert( value);
    }
  
  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  public JsonValue convert( DataValue<?> value)
    {
    return
      Optional.ofNullable( value)
      .map( v -> new Visitor( v).toJson())
      .orElse( null);
    }

  /**
   * Creates the JSON representation of a {@link DataValue}.
   */
  private static class Visitor implements DataValueVisitor
    {
    /**
     * Creates a new Visitor instance.
     */
    public Visitor( DataValue<?> value)
      {
      value_ = value;
      }
    
    /**
     * Returns the JSON representation of this {@link DataValue}
     */
    public JsonValue toJson()
      {
      value_.accept( this);
      return json_;
      }

    public void visit( ArrayValue<?> data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      data.getValue().stream().forEach( item -> builder.add( DataValueJsonValue.toJson( item)));
      json_ = builder.build();
      }

    public void visit( BinaryValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( Base64Domain.encoded( data.getValue()));
      json_ = builder.build().get(0);
      }

    public void visit( BooleanValue data)
      {
      json_ =
        data.getValue().equals( Boolean.TRUE)
        ? JsonValue.TRUE
        : JsonValue.FALSE;
      }

    public void visit( DecimalValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    public void visit( IntegerValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    public void visit( LongValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    public void visit( NullValue data)
      {
      json_ = JsonValue.NULL;
      }

    public void visit( ObjectValue data)
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      data.getValue().keySet().stream().forEach( key -> builder.add( key, DataValueJsonValue.toJson( data.getValue().get( key))));
      json_ = builder.build();
      }

    public void visit( StringValue data)
      {
      JsonArrayBuilder builder = Json.createArrayBuilder();
      builder.add( data.getValue());
      json_ = builder.build().get(0);
      }

    private final DataValue<?> value_;
    private JsonValue json_;
    }
  }
