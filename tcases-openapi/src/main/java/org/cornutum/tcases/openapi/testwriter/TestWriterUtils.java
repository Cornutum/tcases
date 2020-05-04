//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.InvalidStyleException;
import org.cornutum.tcases.openapi.OpenApiUtils;
import org.cornutum.tcases.openapi.resolver.*;
import org.cornutum.tcases.openapi.testwriter.TestWriterUtils.UriEncoder.Component;

import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;

import java.net.URI;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;

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
    try
      {
      return
        OpenApiUtils.ifApplicableStyle(
          param.getStyle(),
          String.valueOf( param.getLocation()).toLowerCase(),
          String.valueOf( param.getType()).toLowerCase());
      }
    catch( InvalidStyleException e)
      {
      throw new TestWriterException( e.getMessage());
      }
    }

  /**
   * Returns the set of request query parameter bindings defined by the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#QUERY QUERY} parameter.
   * All parameter names and values are returned without URI encodeding.
   */
  public static List<Map.Entry<String,String>> getQueryParameters( ParamData param)
    {
    return getQueryParameters( param, false);
    }


  /**
   * Returns the set of request query parameter bindings defined by the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#QUERY QUERY} parameter. 
   */
  public static List<Map.Entry<String,String>> getQueryParameters( ParamData param, boolean uriEncoded)
    {
    try
      {
      if( !param.getLocation().equals( QUERY))
        {
        throw new TestWriterException( String.format( "%s is not a %s parameter", param, QUERY));
        }

      Component component = uriEncoded? Component.QUERY : Component.NONE;
      return FormParameterEncoder.encode( param, component);
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "%s: can't get query parameter values", param), e);
      }
    }

  /**
   * Returns the value of the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#PATH PATH} parameter. 
   * The result is returned without URI encodeding.
   */
  public static String getPathParameterValue( ParamData param)
    {
    return getPathParameterValue( param, false);
    }

  /**
   * Returns the value of the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#PATH PATH} parameter. 
   */
  public static String getPathParameterValue( ParamData param, boolean uriEncoded)
    {
    try
      {
      String style = getValidStyle( param);
      Component component = uriEncoded? Component.PATH : Component.NONE;
      
      return
        "label".equals( style)?
        LabelValueEncoder.encode( param, component) : 

        "matrix".equals( style)?
        MatrixValueEncoder.encode( param, component) : 

        SimpleValueEncoder.encode( param, component);
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "%s: can't get path parameter value", param), e);
      }
    }

  /**
   * Returns the set of request cookie parameter bindings defined by the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#COOKIE COOKIE} parameter.
   * All parameter names and values are URI-encoded if necessary.
   */
  public static List<Map.Entry<String,String>> getCookieParameters( ParamData param)
    {
    try
      {
      if( !param.getLocation().equals( COOKIE))
        {
        throw new TestWriterException( String.format( "%s is not a %s parameter", param, COOKIE));
        }
      
      return FormParameterEncoder.encode( param, Component.NONE);
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "%s: can't get cookie parameter values", param), e);
      }
    }

  /**
   * Returns a string containing the source code for a Java string literal representing the given value.
   */
  public static String stringLiteral( Object value)
    {
    Matcher escapeMatcher = literalEscaped_.matcher( Objects.toString( value, ""));
    StringBuffer escaped = new StringBuffer();
    while( escapeMatcher.find())
      {
      escapeMatcher.appendReplacement( escaped, String.format( "\\\\%s", Matcher.quoteReplacement( escapeMatcher.group())));
    }
    escapeMatcher.appendTail( escaped);

    return String.format( "\"%s\"", escaped.toString());
    }

  /**
   * Returns the name/value pairs that encode a parameter in the "form" style.
   */
  public static class FormParameterEncoder extends UriEncoder implements DataValueVisitor
    {
    /**
     * Creates a new FormParameterEncoder instance.
     */
    private FormParameterEncoder( ParamData param, Component component)
      {
      super( component);
      name_ = param.getName();
      style_ = getValidStyle( param);
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
      if( exploded_)
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

  /**
   * Returns an encoding of a {@link DataValue} in the "simple" style.
   */
  public static class SimpleValueEncoder extends UriEncoder implements DataValueVisitor
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
    
    public void visit( ArrayValue<?> data)
      {
      encoded_ =
        data.getValue().stream()
        .map( item -> SimpleValueEncoder.encode( item, getComponent()))
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
  public static class LabelValueEncoder extends UriEncoder implements DataValueVisitor
    {
    /**
     * Creates a new LabelValueEncoder instance.
     */
    private LabelValueEncoder( DataValue<?> value, boolean exploded, Component component)
      {
      super( component);
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param, Component component)
      {
      return encode( param.getValue(), param.isExploded(), component);
      }

    public static String encode( DataValue<?> value, boolean exploded, Component component)
      {
      return new LabelValueEncoder( value, exploded, component).accepted();
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
      return labelOf( uriEncoded( Objects.toString( value.getValue(), "")));
      }

    private String labelOf( String value)
      {
      return String.format( ".%s", value);
      }
    
    public void visit( ArrayValue<?> data)
      {
      encoded_ =
        data.getValue().stream()
        .map( item -> SimpleValueEncoder.encode( item, getComponent()))
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
          .map( entry -> labelOf( String.format( "%s=%s", uriEncoded( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), getComponent()))))
          .collect( joining());
        }
      else
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .flatMap( entry -> Arrays.asList( uriEncoded( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), getComponent())).stream())
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
  public static class MatrixValueEncoder extends UriEncoder implements DataValueVisitor
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
      return matrixOf( String.format( "%s=%s", uriEncoded( name), uriEncoded( value)));
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
          .map( item -> SimpleValueEncoder.encode( item, Component.NONE))
          .map( this::matrixParamOf)
          .collect( joining());
        }
      else
        {
        encoded_ = matrixParamOf( SimpleValueEncoder.encode( data, Component.NONE));
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
          .map( entry -> matrixParamOf( entry.getKey(), SimpleValueEncoder.encode( entry.getValue(), Component.NONE)))
          .collect( joining());
        }
      else
        {
        encoded_ = matrixParamOf( SimpleValueEncoder.encode( data, Component.NONE));
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

  /**
   * Base class for URI encoders.
   */
  public static abstract class UriEncoder
    {
    public enum Component { NONE, PATH, QUERY };
    
    /**
     * Creates a new UriEncoder instance.
     */
    protected UriEncoder( Component component)
      {
      component_ = component;
      }

    /**
     * Returns the URI component to be encoded
     */
    public Component getComponent()
      {
      return component_;
      }

    /**
     * If encoding is enabled, returns the URI-coded form of the given value.
     * Otherwise, returns the value.
     */
    protected String uriEncoded( String value)
      {
      return uriEncoded( getComponent(), value);
      }

    /**
     * Returns the encoded form of the given value as it would appear in the given URI component.
     */
    public static String uriEncoded( Component component, String value)
      {
      String encoded;
      switch( Optional.ofNullable( component).orElse( Component.NONE))
        {
        case QUERY:
          {
          encoded =
            uriMatcher( null, value).group(2)
            .replaceAll( "\\?", "%3F")
            .replaceAll( "=",   "%3D")
            .replaceAll( "\\&", "%26")
            ;
          break;
          }
        case PATH:
          {
          encoded = uriMatcher( value, null).group(1);
          break;
          }
        case NONE:
          {
          encoded = value;
          break;
          }
        default:
          {
          throw new IllegalArgumentException( String.format( "Unknown component=%s", component));
          }
        }

      return encoded;
      }

    private static Matcher uriMatcher( String path, String query)
      {
      try
        {
        String uri =
          new URI(
            "http",
            null,
            "host",
            -1,
            "/" + Optional.ofNullable( path).orElse( "path"),
            Optional.ofNullable( query).orElse( "query"),
            null)
          .toASCIIString();
        
        Matcher matcher = uriPattern_.matcher( uri);
        if( !matcher.matches())
          {
          throw new IllegalArgumentException( String.format( "Can't recognize uri=%s", uri));
          }
        
        return matcher;
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( String.format( "Can't encode path='%s', query='%s'", path, query), e);
        }
      }

    private final Component component_;
    private static final Pattern uriPattern_ = Pattern.compile( "http://host/([^?]*)\\?(.*)");
    }

  private static final Pattern literalEscaped_ = Pattern.compile( "[\\\\\"]");
  }
