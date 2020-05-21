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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.AbstractMap.SimpleEntry;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;

/**
 * Returns the name/value pairs that encode a parameter in the "form" style.
 */
public class FormParameterEncoder extends UriEncoder implements DataValueVisitor
  {
  /**
   * Creates a new FormParameterEncoder instance.
   */
  private FormParameterEncoder( ParamData param, Component component)
    {
    super( component);
    name_ = param.getName();
    style_ = param.getStyle();
    exploded_ = param.isExploded();
    value_ = param.getValue();
    }

  public static List<Map.Entry<String,String>> encode( ParamData param, Component component)
    {
    return new FormParameterEncoder( param, component).accepted();
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

  private void bind( String name, Object value)
    {
    add( uriEncoded( name), uriEncoded( Objects.toString( value, "")));
    }

  private void bindDeep( String property, Object value)
    {
    add( String.format( "%s[%s]", uriEncoded( name_), uriEncoded( property)), uriEncoded( Objects.toString( value, "")));
    }

  private void bindParam( Object value)
    {
    bind( name_, value);
    }

  private void bindParam( DataValue<?> value)
    {
    bindParam( value.getValue());
    }
    
  public void visit( ArrayValue<?> data)
    {
    if( data.getValue().isEmpty())
      {
      bindParam( "");
      }
    else if( exploded_)
      {
      data.getValue().stream()
        .forEach( value -> bindParam( SimpleValueEncoder.encode( value, Component.NONE)));
      }
    else
      {
      String delim = "pipeDelimited".equals( style_)? "|" : "spaceDelimited".equals( style_)? " " : ",";

      bindParam(
        data.getValue().stream()
        .map( item -> SimpleValueEncoder.encode( item, Component.NONE))
        .collect( joining( delim)));
      }
    }

  public void visit( BinaryValue data)
    {
    bindParam( Base64Domain.encoded( data.getValue()));
    }

  public void visit( BooleanValue data)
    {
    bindParam( data);
    }

  public void visit( DecimalValue data)
    {
    bindParam( data);
    }

  public void visit( IntegerValue data)
    {
    bindParam( data);
    }

  public void visit( LongValue data)
    {
    bindParam( data);
    }

  public void visit( NullValue data)
    {
    bindParam( data);
    }

  public void visit( ObjectValue data)
    {
    if( "deepObject".equals( style_))
      {
      data.getValue()
        .forEach( (property, value) -> bindDeep( property, SimpleValueEncoder.encode( value, Component.NONE)));
      }
    else if( exploded_)
      {
      data.getValue()
        .forEach( (property, value) -> bind( property, SimpleValueEncoder.encode( value, Component.NONE)));
      }
    else
      {
      bindParam(
        data.getValue().entrySet().stream()
        .flatMap( entry -> Arrays.asList( entry.getKey(), SimpleValueEncoder.encode( entry.getValue(), Component.NONE)).stream())
        .collect( joining( ",")));
      }
    }

  public void visit( StringValue data)
    {
    bindParam( data);
    }
  
  private final String name_;
  private final String style_;
  private final boolean exploded_;
  private final DataValue<?> value_;
  private List<Map.Entry<String,String>> bindings_ = new ArrayList<Map.Entry<String,String>>();
  }
