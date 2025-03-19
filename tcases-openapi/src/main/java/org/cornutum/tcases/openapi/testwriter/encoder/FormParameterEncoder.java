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
    this( param.getName(), param.getStyle(), param.isExploded(), param.getValue(), component);
    }
  
  /**
   * Creates a new FormParameterEncoder instance.
   */
  private FormParameterEncoder( String name, String style, boolean exploded, DataValue<?> value, Component component)
    {
    super( component);
    name_ = name;
    style_ = style;
    exploded_ = exploded;
    value_ = value;
    }

  /**
   * Returns the name/value pairs that encode a parameter in the "form" style.
   */
  public static List<Map.Entry<String,String>> encode( ParamData param, Component component)
    {
    return new FormParameterEncoder( param, component).accepted();
    }

  /**
   * Returns the name/value pairs that encode a property value in the "form" style.
   */
  public static List<Map.Entry<String,String>> encode( String name, String style, boolean exploded, DataValue<?> value)
    {
    return new FormParameterEncoder( name, style, exploded, value, Component.NONE).accepted();
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
    add( uriEncoded( name), uriEncoded( Objects.toString( value, null)));
    }

  private void bindMember( String name, DataValue<?> member)
    {
    bind(
      name,
      member.getValue() == null? null : SimpleValueEncoder.encode( member, Component.NONE));
    }

  private void bindDeep( String property, DataValue<?> member)
    {
    bindMember( String.format( "%s[%s]", name_, property), member);
    }

  private void bindParam( Object value)
    {
    bind( name_, value);
    }

  private void bindParam( DataValue<?> value)
    {
    bindParam( value.getValue());
    }
    
  @Override
  public void visit( ArrayValue<?> data)
    {
    if( data.getValue().isEmpty())
      {
      bindParam( "");
      }
    else if( exploded_)
      {
      data.getValue().stream()
        .forEach( value -> bindMember( name_, value));
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

  @Override
  public void visit( BinaryValue data)
    {
    bindParam( Base64Domain.encoded( data.getValue()));
    }

  @Override
  public void visit( BooleanValue data)
    {
    bindParam( data);
    }

  @Override
  public void visit( DecimalValue data)
    {
    bindParam( data);
    }

  @Override
  public void visit( IntegerValue data)
    {
    bindParam( data);
    }

  @Override
  public void visit( LongValue data)
    {
    bindParam( data);
    }

  @Override
  public void visit( NullValue data)
    {
    bindParam( data);
    }

  @Override
  public void visit( ObjectValue data)
    {
    if( !exploded_)
      {
      String delim = "pipeDelimited".equals( style_)? "|" : "spaceDelimited".equals( style_)? " " : ",";
      
      bindParam(
        data.getValue().entrySet().stream()
        .flatMap( entry -> Arrays.asList( entry.getKey(), SimpleValueEncoder.encode( entry.getValue(), Component.NONE)).stream())
        .collect( joining( delim)));
      }
    else if( "deepObject".equals( style_))
      {
      data.getValue()
        .forEach( (property, value) -> bindDeep( property, value));
      }
    else
      {
      data.getValue()
        .forEach( (property, value) -> bindMember( property, value));
      }
    }

  @Override
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
