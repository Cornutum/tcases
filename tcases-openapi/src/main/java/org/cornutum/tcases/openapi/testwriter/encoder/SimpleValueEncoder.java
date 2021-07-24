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
import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.resolver.StringValue;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import static java.util.stream.Collectors.joining;

/**
 * Returns an encoding of a {@link DataValue} in the "simple" style.
 */
public class SimpleValueEncoder extends UriEncoder implements DataValueVisitor
  {
  /**
   * Creates a new SimpleValueEncoder instance.
   */
  private SimpleValueEncoder( DataValue<?> value, boolean exploded, Component component)
    {
    super( component);
    value_ = value;
    exploded_ = exploded;
    }

  public static String encode( ParamData param, Component component)
    {
    return encode( param.getValue(), param.isExploded(), component);
    }

  public static String encode( DataValue<?> value, Component component)
    {
    return encode( value, false, component);
    }

  public static String encode( DataValue<?> value, boolean exploded, Component component)
    {
    return new SimpleValueEncoder( value, exploded, component).accepted();
    }

  private String accepted()
    {
    return
      Optional.ofNullable( value_)
      .map( value -> { 
        value.accept( this);
        return encoded_;
        })
      .orElse( "");
    }

  private String stringOf( DataValue<?> value)
    {
    return uriEncoded( Objects.toString( value.getValue(), ""));
    }
    
  @Override
  public void visit( ArrayValue<?> data)
    {
    encoded_ =
      data.getValue().stream()
      .map( item -> SimpleValueEncoder.encode( item, getComponent()))
      .collect( joining( ","));
    }

  @Override
  public void visit( BinaryValue data)
    {
    encoded_ = Base64Domain.encoded( data.getValue());
    }

  @Override
  public void visit( BooleanValue data)
    {
    encoded_ = stringOf( data);
    }

  @Override
  public void visit( DecimalValue data)
    {
    encoded_ = stringOf( data);
    }

  @Override
  public void visit( IntegerValue data)
    {
    encoded_ = stringOf( data);
    }

  @Override
  public void visit( LongValue data)
    {
    encoded_ = stringOf( data);
    }

  @Override
  public void visit( NullValue data)
    {
    encoded_ = stringOf( data);
    }

  @Override
  public void visit( ObjectValue data)
    {
    if( exploded_)
      {
      encoded_ = 
        data.getValue().entrySet().stream()
        .map( entry -> String.format( "%s=%s", uriEncoded( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), getComponent())))
        .collect( joining( ","));
      }
    else
      {
      encoded_ = 
        data.getValue().entrySet().stream()
        .flatMap( entry -> Arrays.asList( uriEncoded( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), getComponent())).stream())
        .collect( joining( ","));
      }
    }

  @Override
  public void visit( StringValue data)
    {
    encoded_ = stringOf( data);
    }
  
  private final boolean exploded_;
  private final DataValue<?> value_;
  private String encoded_;
  }
