//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.*;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type.*;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;

import java.net.URLEncoder;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Define common methods for representing request test definitions in API tests.
 */
public final class TestWriterUtils
  {
  /**
   * Creates a new TestWriterUtils instance.
   */
  private TestWriterUtils()
    {
    // Static methods only
    }

  /**
   * Returns the validated style attribute for the given parameter. When the specified style
   * is not applicable for this parameter, throws a TestWriterException.
   */
  public static String getValidStyle( ParamData param)
    {
    String style = param.getStyle();
    DataValue.Type type = param.getType();

    if( type != null)
      {
      switch( param.getLocation())
        {
        case QUERY:
        case COOKIE:
          {
          if( "deepObject".equals( style))
            {
            if( !(type.equals( OBJECT) || type.equals( NULL)))
              {
              throw new TestWriterException( String.format( "Style=%s is not applicable for data type=%s", style, type));
              }
            }
          else if( "pipeDelimited".equals( style) || "spaceDelimited".equals( style))
            {
            if( !(type.equals( ARRAY) || type.equals( NULL)))
              {
              throw new TestWriterException( String.format( "Style=%s is not applicable for data type=%s", style, type));
              }
            }
          else if( !"form".equals( style))
            {
            throw new TestWriterException( String.format( "Style=%s is not applicable for a %s parameter", style, param.getLocation()));
            }
          break;
          }

        case PATH:
          {
          if( !("simple".equals( style) || "matrix".equals( style) || "label".equals( style)))
            {
            throw new TestWriterException( String.format( "Style=%s is not applicable for a %s parameter", style, param.getLocation()));
            }
          break;
          }

        case HEADER:
          {
          if( !"simple".equals( style))
            {
            throw new TestWriterException( String.format( "Style=%s is not applicable for a %s parameter", style, param.getLocation()));
            }
          break;
          }
        }
      }

    return style;
    }

  /**
   * Returns the set of request query parameter bindings defined by the given {@link ParamDef.Location#QUERY QUERY} parameter. 
   */
  public static List<Map.Entry<String,String>> getQueryParameters( ParamData param, boolean urlEncoded)
    {
    List<Map.Entry<String,String>> bindings = QueryParameterEncoder.encode( param);
    if( urlEncoded)
      {
      bindings = 
        bindings.stream()
        .map( binding -> {
            try
              {
              String encodedKey = URLEncoder.encode( binding.getKey(), "UTF-8");
              String encodedValue = URLEncoder.encode( binding.getValue(), "UTF-8");
              return new SimpleEntry<String,String>( encodedKey, encodedValue);
              }
            catch( Exception e)
              {
              throw new TestWriterException( String.format( "%: can't encode parameter=%s", param, binding), e);
              }})
        .collect( toList());
      }
    
    return bindings;
    }

  /**
   * Returns the set of request query parameter bindings defined by the given {@link ParamDef.Location#QUERY QUERY} parameter. 
   */
  public static String getPathParameterValue( ParamData param)
    {
    String style = getValidStyle( param);
    
    return
      "label".equals( style)?
      LabelValueEncoder.encode( param) : 

      "matrix".equals( style)?
      MatrixValueEncoder.encode( param) : 

      SimpleValueEncoder.encode( param);
    }

  /**
   * Returns a set of request query parameter bindings defined by the given {@link ParamDef.Location#QUERY} parameter. 
   */
  public static class QueryParameterEncoder implements DataValueVisitor
    {
    /**
     * Creates a new QueryParameterEncoder instance.
     */
    private QueryParameterEncoder( ParamData param)
      {
      if( !param.getLocation().equals( QUERY))
        {
        throw new TestWriterException( String.format( "%s is not a %s parameter", param, QUERY));
        }

      name_ = param.getName();
      style_ = getValidStyle( param);
      exploded_ = param.isExploded();
      value_ = param.getValue();
      }

    public static List<Map.Entry<String,String>> encode( ParamData param)
      {
      return new QueryParameterEncoder( param).accepted();
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

    private void bind( String name, Object value)
      {
      bindings_.add( new SimpleEntry<String,String>( name, Objects.toString( value, "")));
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
      if( exploded_)
        {
        data.getValue().stream()
          .forEach( value -> bindParam( SimpleValueEncoder.encode( value)));
        }
      else
        {
        String delim = "pipeDelimited".equals( style_)? "|" : "spaceDelimited".equals( style_)? " " : ",";

        bindParam(
          data.getValue().stream()
          .map( SimpleValueEncoder::encode)
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
          .forEach( (property, value) -> bind( String.format( "%s[%s]", name_, property), SimpleValueEncoder.encode( value)));
        }
      else
        {
        if( exploded_)
          {
          data.getValue()
            .forEach( (property, value) -> bind( property, SimpleValueEncoder.encode( value)));
          }
        else
          {
          bindParam(
            data.getValue().entrySet().stream()
            .flatMap( entry -> Arrays.asList( entry.getKey(), SimpleValueEncoder.encode( entry.getValue())).stream())
            .collect( joining( ",")));
          }
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

  /**
   * Returns an encoding of a {@link DataValue} in the "simple" style.
   */
  public static class SimpleValueEncoder implements DataValueVisitor
    {
    /**
     * Creates a new SimpleValueEncoder instance.
     */
    private SimpleValueEncoder( DataValue<?> value, boolean exploded)
      {
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param)
      {
      return encode( param.getValue(), param.isExploded());
      }

    public static String encode( DataValue<?> value)
      {
      return encode( value, false);
      }

    public static String encode( DataValue<?> value, boolean exploded)
      {
      return new SimpleValueEncoder( value, exploded).accepted();
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
      return Objects.toString( value.getValue(), "");
      }
    
    public void visit( ArrayValue<?> data)
      {
      encoded_ =
        data.getValue().stream()
        .map( SimpleValueEncoder::encode)
        .collect( joining( ","));
      }

    public void visit( BinaryValue data)
      {
      encoded_ = Base64Domain.encoded( data.getValue());
      }

    public void visit( BooleanValue data)
      {
      encoded_ = stringOf( data);
      }

    public void visit( DecimalValue data)
      {
      encoded_ = stringOf( data);
      }

    public void visit( IntegerValue data)
      {
      encoded_ = stringOf( data);
      }

    public void visit( LongValue data)
      {
      encoded_ = stringOf( data);
      }

    public void visit( NullValue data)
      {
      encoded_ = stringOf( data);
      }

    public void visit( ObjectValue data)
      {
      if( exploded_)
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .map( entry -> String.format( "%s=%s", entry.getKey(), SimpleValueEncoder.encode( entry.getValue())))
          .collect( joining( ","));
        }
      else
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .flatMap( entry -> Arrays.asList( entry.getKey(), SimpleValueEncoder.encode( entry.getValue())).stream())
          .collect( joining( ","));
        }
      }

    public void visit( StringValue data)
      {
      encoded_ = stringOf( data);
      }
  
    private final boolean exploded_;
    private final DataValue<?> value_;
    private String encoded_;
    }

  /**
   * Returns an encoding of a {@link DataValue} in the "label" style.
   */
  public static class LabelValueEncoder implements DataValueVisitor
    {
    /**
     * Creates a new LabelValueEncoder instance.
     */
    private LabelValueEncoder( DataValue<?> value, boolean exploded)
      {
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param)
      {
      return encode( param.getValue(), param.isExploded());
      }

    public static String encode( DataValue<?> value, boolean exploded)
      {
      return new LabelValueEncoder( value, exploded).accepted();
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

    private String labelOf( DataValue<?> value)
      {
      return labelOf( Objects.toString( value.getValue(), ""));
      }

    private String labelOf( String value)
      {
      return String.format( ".%s", value);
      }
    
    public void visit( ArrayValue<?> data)
      {
      encoded_ =
        data.getValue().stream()
        .map( SimpleValueEncoder::encode)
        .map( this::labelOf)
        .collect( joining());
      }

    public void visit( BinaryValue data)
      {
      encoded_ = labelOf( Base64Domain.encoded( data.getValue()));
      }

    public void visit( BooleanValue data)
      {
      encoded_ = labelOf( data);
      }

    public void visit( DecimalValue data)
      {
      encoded_ = labelOf( data);
      }

    public void visit( IntegerValue data)
      {
      encoded_ = labelOf( data);
      }

    public void visit( LongValue data)
      {
      encoded_ = labelOf( data);
      }

    public void visit( NullValue data)
      {
      encoded_ = labelOf( data);
      }

    public void visit( ObjectValue data)
      {
      if( exploded_)
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .map( entry -> labelOf( String.format( "%s=%s", entry.getKey(), SimpleValueEncoder.encode( entry.getValue()))))
          .collect( joining());
        }
      else
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .flatMap( entry -> Arrays.asList( entry.getKey(), SimpleValueEncoder.encode( entry.getValue())).stream())
          .map( this::labelOf)
          .collect( joining());
        }
      }

    public void visit( StringValue data)
      {
      encoded_ = labelOf( data);
      }
  
    private final boolean exploded_;
    private final DataValue<?> value_;
    private String encoded_;
    }

  /**
   * Returns an encoding of a {@link DataValue} in the "matrix" style.
   */
  public static class MatrixValueEncoder implements DataValueVisitor
    {
    /**
     * Creates a new MatrixValueEncoder instance.
     */
    private MatrixValueEncoder( String name, DataValue<?> value, boolean exploded)
      {
      name_ = name;
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param)
      {
      return encode( param.getName(), param.getValue(), param.isExploded());
      }

    public static String encode( String name, DataValue<?> value, boolean exploded)
      {
      return new MatrixValueEncoder( name, value, exploded).accepted();
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
      return matrixOf( String.format( "%s=%s", name_, value));
      }

    private String matrixOf( String value)
      {
      return String.format( ";%s", value);
      }
    
    public void visit( ArrayValue<?> data)
      {
      if( exploded_)
        {
        encoded_ =
          data.getValue().stream()
          .map( SimpleValueEncoder::encode)
          .map( this::matrixParamOf)
          .collect( joining());
        }
      else
        {
        encoded_ = matrixParamOf( SimpleValueEncoder.encode( data));
        }
      }

    public void visit( BinaryValue data)
      {
      encoded_ = matrixParamOf( Base64Domain.encoded( data.getValue()));
      }

    public void visit( BooleanValue data)
      {
      encoded_ = matrixParamOf( data);
      }

    public void visit( DecimalValue data)
      {
      encoded_ = matrixParamOf( data);
      }

    public void visit( IntegerValue data)
      {
      encoded_ = matrixParamOf( data);
      }

    public void visit( LongValue data)
      {
      encoded_ = matrixParamOf( data);
      }

    public void visit( NullValue data)
      {
      encoded_ = matrixOf( name_);
      }

    public void visit( ObjectValue data)
      {
      if( exploded_)
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .map( entry -> matrixOf( String.format( "%s=%s", entry.getKey(), SimpleValueEncoder.encode( entry.getValue()))))
          .collect( joining());
        }
      else
        {
        encoded_ = matrixParamOf( SimpleValueEncoder.encode( data));
        }
      }

    public void visit( StringValue data)
      {
      encoded_ = matrixParamOf( data);
      }

    private final String name_;
    private final boolean exploded_;
    private final DataValue<?> value_;
    private String encoded_;
    }

  }
