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
   * All parameter names and values are URI-encoded if necessary.
   */
  public static List<Map.Entry<String,String>> getQueryParameters( ParamData param)
    {
    return getQueryParameters( param, true);
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
      
      return FormParameterEncoder.encode( param, uriEncoded);
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "%s: can't get query parameter values", param), e);
      }
    }

  /**
   * Returns the value of the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#PATH PATH} parameter. 
   * The result is URI-encoded if necessary.
   */
  public static String getPathParameterValue( ParamData param)
    {
    return getPathParameterValue( param, true);
    }

  /**
   * Returns the value of the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#PATH PATH} parameter. 
   */
  public static String getPathParameterValue( ParamData param, boolean uriEncoded)
    {
    try
      {
      String style = getValidStyle( param);

      return
        "label".equals( style)?
        LabelValueEncoder.encode( param, uriEncoded) : 

        "matrix".equals( style)?
        MatrixValueEncoder.encode( param, uriEncoded) : 

        SimpleValueEncoder.encode( param, uriEncoded);
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
      
      return FormParameterEncoder.encode( param, false);
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "%s: can't get cookie parameter values", param), e);
      }
    }

  /**
   * Returns the URI-encoded form of the given value.
   */
  public static String uriEncoded( String value)
    {
    try
      {
      String uri = new URI( "http", null, "/" + value, null).toASCIIString();
      return uri.substring( 6);
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( String.format( "Can't encode value='%s'", value), e);
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
     * Creates a new QueryParameterEncoder instance.
     */
    private FormParameterEncoder( ParamData param, boolean uriEncoded)
      {
      super( uriEncoded);
      name_ = param.getName();
      style_ = getValidStyle( param);
      exploded_ = param.isExploded();
      value_ = param.getValue();
      }

    public static List<Map.Entry<String,String>> encode( ParamData param, boolean uriEncoded)
      {
      return new FormParameterEncoder( param, uriEncoded).accepted();
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
      add( uriOf( name), uriOf( Objects.toString( value, "")));
      }

    private void bindDeep( String property, Object value)
      {
      add( String.format( "%s[%s]", uriOf( name_), uriOf( property)), uriOf( Objects.toString( value, "")));
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
          .forEach( value -> bindParam( SimpleValueEncoder.encode( value, false)));
        }
      else
        {
        String delim = "pipeDelimited".equals( style_)? "|" : "spaceDelimited".equals( style_)? " " : ",";

        bindParam(
          data.getValue().stream()
          .map( item -> SimpleValueEncoder.encode( item, false))
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
          .forEach( (property, value) -> bindDeep( property, SimpleValueEncoder.encode( value, false)));
        }
      else if( exploded_)
        {
        data.getValue()
          .forEach( (property, value) -> bind( property, SimpleValueEncoder.encode( value, false)));
        }
      else
        {
        bindParam(
          data.getValue().entrySet().stream()
          .flatMap( entry -> Arrays.asList( entry.getKey(), SimpleValueEncoder.encode( entry.getValue(), false)).stream())
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
    private SimpleValueEncoder( DataValue<?> value, boolean exploded, boolean uriEncoded)
      {
      super( uriEncoded);
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param, boolean uriEncoded)
      {
      return encode( param.getValue(), param.isExploded(), uriEncoded);
      }

    public static String encode( DataValue<?> value, boolean uriEncoded)
      {
      return encode( value, false, uriEncoded);
      }

    public static String encode( DataValue<?> value, boolean exploded, boolean uriEncoded)
      {
      return new SimpleValueEncoder( value, exploded, uriEncoded).accepted();
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
      return uriOf( Objects.toString( value.getValue(), ""));
      }
    
    public void visit( ArrayValue<?> data)
      {
      encoded_ =
        data.getValue().stream()
        .map( item -> SimpleValueEncoder.encode( item, isUriEncoded()))
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
          .map( entry -> String.format( "%s=%s", uriOf( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), isUriEncoded())))
          .collect( joining( ","));
        }
      else
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .flatMap( entry -> Arrays.asList( uriOf( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), isUriEncoded())).stream())
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
    private LabelValueEncoder( DataValue<?> value, boolean exploded, boolean uriEncoded)
      {
      super( uriEncoded);
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param, boolean uriEncoded)
      {
      return encode( param.getValue(), param.isExploded(), uriEncoded);
      }

    public static String encode( DataValue<?> value, boolean exploded, boolean uriEncoded)
      {
      return new LabelValueEncoder( value, exploded, uriEncoded).accepted();
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
      return labelOf( uriOf( Objects.toString( value.getValue(), "")));
      }

    private String labelOf( String value)
      {
      return String.format( ".%s", value);
      }
    
    public void visit( ArrayValue<?> data)
      {
      encoded_ =
        data.getValue().stream()
        .map( item -> SimpleValueEncoder.encode( item, isUriEncoded()))
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
          .map( entry -> labelOf( String.format( "%s=%s", uriOf( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), isUriEncoded()))))
          .collect( joining());
        }
      else
        {
        encoded_ = 
          data.getValue().entrySet().stream()
          .flatMap( entry -> Arrays.asList( uriOf( entry.getKey()), SimpleValueEncoder.encode( entry.getValue(), isUriEncoded())).stream())
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
    private MatrixValueEncoder( String name, DataValue<?> value, boolean exploded, boolean uriEncoded)
      {
      super( uriEncoded);
      name_ = name;
      value_ = value;
      exploded_ = exploded;
      }

    public static String encode( ParamData param, boolean uriEncoded)
      {
      return encode( param.getName(), param.getValue(), param.isExploded(), uriEncoded);
      }

    public static String encode( String name, DataValue<?> value, boolean exploded, boolean uriEncoded)
      {
      return new MatrixValueEncoder( name, value, exploded, uriEncoded).accepted();
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
      return matrixOf( String.format( "%s=%s", uriOf( name), uriOf( value)));
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
          .map( item -> SimpleValueEncoder.encode( item, false))
          .map( this::matrixParamOf)
          .collect( joining());
        }
      else
        {
        encoded_ = matrixParamOf( SimpleValueEncoder.encode( data, false));
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
          .map( entry -> matrixParamOf( entry.getKey(), SimpleValueEncoder.encode( entry.getValue(), false)))
          .collect( joining());
        }
      else
        {
        encoded_ = matrixParamOf( SimpleValueEncoder.encode( data, false));
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
  private static class UriEncoder
    {
    /**
     * Creates a new UriEncoder instance.
     */
    protected UriEncoder( boolean uriEncoded)
      {
      uriEncoded_ = uriEncoded;
      }

    /**
     * Returns if URI encoding is enabled for this encoder.
     */
    public boolean isUriEncoded()
      {
      return uriEncoded_;
      }

    /**
     * If encoding is enabled, returns the URI-coded form of the given value.
     * Otherwise, returns the value.
     */
    protected String uriOf( String value)
      {
      return
        uriEncoded_
        ? uriEncoded( value)
        : value;
      }

    private final boolean uriEncoded_;
    }

  private static final Pattern literalEscaped_ = Pattern.compile( "[\\\\\"]");
  }
