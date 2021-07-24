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
import org.cornutum.tcases.openapi.testwriter.encoder.UriEncoder.Component;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;

import java.net.URLEncoder;

/**
 * Returns the name/value pairs that encode a {@link DataValue} for the <CODE>application/x-www-form-urlencoded</CODE> media type.
 */
public class FormUrlEncoder implements DataValueVisitor
  {
  /**
   * Creates a new FormUrlEncoder instance.
   */
  private FormUrlEncoder( DataValue<?> value, boolean encoded)
    {
    value_ = value;
    encoded_ = encoded;
    }

  /**
   * Returns the name/value pairs that encode the given {@link DataValue}. If <CODE>encoded</CODE>
   * is true, apply the <CODE>application/x-www-form-urlencoded</CODE> encoding to all pairs.
   */
  public static List<Map.Entry<String,String>> encode( DataValue<?> value, boolean encoded)
    {
    return new FormUrlEncoder( value, encoded).accepted();
    }

  /**
   * Returns the name/value pairs that encode the given {@link DataValue}, applying
   * the <CODE>application/x-www-form-urlencoded</CODE> encoding to all pairs.
   */
  public static List<Map.Entry<String,String>> encode( DataValue<?> value)
    {
    return encode( value, true);
    }

  /**
   * Returns the full encoding of the content of the given {@link DataValue}.
   */
  public static String toForm( DataValue<?> value)
    {
    return
      encode( value).stream()
      .map( entry -> String.format( "%s=%s", entry.getKey(), entry.getValue()))
      .collect( joining( "&"));
    }

  private List<Map.Entry<String,String>> accepted()
    {
    return
      Optional.ofNullable( value_)
      .map( value -> { 
        value.accept( this);
        return bindings_;
        })
      .orElse( emptyList());
    }

  private void add( String name, String value)
    {
    bindings_.add( new SimpleEntry<String,String>( name, value));
    }

  private void bind( String name, DataValue<?> data)
    {
    bind( name, data.getValue());
    }

  private void bind( String name, Object value)
    {
    add( urlEncoded( name), urlEncoded( Objects.toString( value, "")));
    }

  private String urlEncoded( String value)
    {
    try
      {
      return
        encoded_
        ? URLEncoder.encode( value, "UTF-8")
        : value;
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( String.format( "Can't get URL encoding for value='%s'", value), e);
      }
    }
  
  @Override
  public void visit( ArrayValue<?> data)
    {
    IntStream.range( 0, data.getValue().size()).forEach( i -> bind( String.valueOf(i), SimpleValueEncoder.encode( data.getValue().get(i), Component.NONE)));
    }

  @Override
  public void visit( BinaryValue data)
    {
    bind( "bytes", Base64Domain.encoded( data.getValue()));
    }

  @Override
  public void visit( BooleanValue data)
    {
    bind( "boolean", data);
    }

  @Override
  public void visit( DecimalValue data)
    {
    bind( "number", data);
    }

  @Override
  public void visit( IntegerValue data)
    {
    bind( "integer", data);
    }

  @Override
  public void visit( LongValue data)
    {
    bind( "integer", data);
    }

  @Override
  public void visit( NullValue data)
    {
    }

  @Override
  public void visit( ObjectValue data)
    {
    data.getValue().forEach( (property,value) -> bind( property, SimpleValueEncoder.encode( value, Component.NONE)));
    }

  @Override
  public void visit( StringValue data)
    {
    bind( "string", data);
    }
  
  private final DataValue<?> value_;
  private final boolean encoded_;
  private final List<Map.Entry<String,String>> bindings_ = new ArrayList<Map.Entry<String,String>>();
  }
