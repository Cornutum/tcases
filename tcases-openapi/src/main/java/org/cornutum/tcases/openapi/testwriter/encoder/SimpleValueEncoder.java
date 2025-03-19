//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.resolve.ArrayValue;
import org.cornutum.tcases.resolve.Base64Domain;
import org.cornutum.tcases.resolve.BinaryValue;
import org.cornutum.tcases.resolve.BooleanValue;
import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.resolve.DataValueVisitor;
import org.cornutum.tcases.resolve.DecimalValue;
import org.cornutum.tcases.resolve.IntegerValue;
import org.cornutum.tcases.resolve.LongValue;
import org.cornutum.tcases.resolve.NullValue;
import org.cornutum.tcases.resolve.ObjectValue;
import org.cornutum.tcases.resolve.StringValue;

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

  /**
   * Returns an encoding of a parameter in the "simple" style.
   */
  public static String encode( ParamData param, Component component)
    {
    return encode( param.getValue(), param.isExploded(), component);
    }

  /**
   * Returns an encoding of a {@link DataValue} in the "simple" style.
   */
  public static String encode( DataValue<?> value, Component component)
    {
    return encode( value, false, component);
    }

  /**
   * Returns an encoding of a {@link DataValue} in the "simple" style for {@link UriEncoder.Component Component.NONE}.
   */
  public static String encode( DataValue<?> value, boolean exploded)
    {
    return encode( value, exploded, Component.NONE);
    }

  /**
   * Returns an encoding of a {@link DataValue} in the "simple" style.
   */
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
