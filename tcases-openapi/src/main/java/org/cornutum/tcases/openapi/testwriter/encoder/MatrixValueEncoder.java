//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.Base64Domain;
import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.resolve.ArrayValue;
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

import java.util.Objects;
import java.util.Optional;
import static java.util.stream.Collectors.joining;

/**
 * Returns an encoding of a {@link DataValue} in the "matrix" style.
 */
public class MatrixValueEncoder extends UriEncoder implements DataValueVisitor
  {
  /**
   * Creates a new MatrixValueEncoder instance.
   */
  private MatrixValueEncoder( String name, DataValue<?> value, boolean exploded, Component component)
    {
    super( component);
    name_ = name;
    value_ = value;
    exploded_ = exploded;
    }

  public static String encode( ParamData param, Component component)
    {
    return encode( param.getName(), param.getValue(), param.isExploded(), component);
    }

  public static String encode( String name, DataValue<?> value, boolean exploded, Component component)
    {
    return new MatrixValueEncoder( name, value, exploded, component).accepted();
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

  private String matrixParamOf( DataValue<?> value)
    {
    return matrixParamOf( Objects.toString( value.getValue(), ""));
    }

  private String matrixParamOf( String value)
    {
    return matrixParamOf( name_, value);
    }

  private String matrixParamOf( String name, String value)
    {
    return
      value.isEmpty()
      ? matrixOf( uriEncoded( name))
      : matrixOf( String.format( "%s=%s", uriEncoded( name), uriEncoded( value)));
    }

  private String matrixOf( String value)
    {
    return String.format( ";%s", value);
    }
    
  @Override
  public void visit( ArrayValue<?> data)
    {
    if( data.getValue().isEmpty())
      {
      encoded_ = matrixParamOf( "");
      }
    else if( exploded_)
      {
      encoded_ =
        data.getValue().stream()
        .map( item -> SimpleValueEncoder.encode( item, Component.NONE))
        .map( this::matrixParamOf)
        .collect( joining());
      }
    else
      {
      encoded_ = matrixParamOf( SimpleValueEncoder.encode( data, Component.NONE));
      }
    }

  @Override
  public void visit( BinaryValue data)
    {
    encoded_ = matrixParamOf( Base64Domain.encoded( data.getValue()));
    }

  @Override
  public void visit( BooleanValue data)
    {
    encoded_ = matrixParamOf( data);
    }

  @Override
  public void visit( DecimalValue data)
    {
    encoded_ = matrixParamOf( data);
    }

  @Override
  public void visit( IntegerValue data)
    {
    encoded_ = matrixParamOf( data);
    }

  @Override
  public void visit( LongValue data)
    {
    encoded_ = matrixParamOf( data);
    }

  @Override
  public void visit( NullValue data)
    {
    encoded_ = "";
    }

  @Override
  public void visit( ObjectValue data)
    {
    if( exploded_)
      {
      encoded_ = 
        data.getValue().entrySet().stream()
        .map( entry -> matrixParamOf( entry.getKey(), SimpleValueEncoder.encode( entry.getValue(), Component.NONE)))
        .collect( joining());
      }
    else
      {
      encoded_ = matrixParamOf( SimpleValueEncoder.encode( data, Component.NONE));
      }
    }

  @Override
  public void visit( StringValue data)
    {
    encoded_ = matrixParamOf( data);
    }

  private final String name_;
  private final boolean exploded_;
  private final DataValue<?> value_;
  private String encoded_;
  }
