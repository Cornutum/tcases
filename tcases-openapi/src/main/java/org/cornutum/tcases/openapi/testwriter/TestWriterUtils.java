//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.*;
import org.cornutum.tcases.openapi.resolver.io.FormParameterEncoder;
import org.cornutum.tcases.openapi.resolver.io.LabelValueEncoder;
import org.cornutum.tcases.openapi.resolver.io.MatrixValueEncoder;
import org.cornutum.tcases.openapi.resolver.io.SimpleValueEncoder;
import org.cornutum.tcases.openapi.resolver.io.UriEncoder.Component;

import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
      if( !param.getLocation().equals( PATH))
        {
        throw new TestWriterException( String.format( "%s is not a %s parameter", param, PATH));
        }
      
      String style = param.getStyle();
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
   * Returns the value of the given {@link org.cornutum.tcases.openapi.resolver.ParamDef.Location#HEADER HEADER} parameter.
   * Returns <CODE>Optional.empty()</CODE> if no value is defined for this parameter.
   */
  public static Optional<String> getHeaderParameterValue( ParamData param)
    {
    try
      {
      if( !param.getLocation().equals( HEADER))
        {
        throw new TestWriterException( String.format( "%s is not a %s parameter", param, HEADER));
        }
      
      return
        Optional.of( param.getStyle())
        .filter( style -> "simple".equals( style))
        .map( style -> Optional.ofNullable( param.getValue()).map( v -> quoted( SimpleValueEncoder.encode( param, Component.NONE))))
        .orElseThrow( () -> new IllegalStateException( String.format( "style=%s is not applicable for a HEADER parameter", param.getStyle())));
      }
    catch( Exception e)
      {
      throw new TestWriterException( String.format( "%s: can't get header parameter value", param), e);
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
   * If the given value contains leading or trailing whitespace, returns the given value enclosed in double quotes.
   * Otherwise, returns the given value.
   */
  private static String quoted( String value)
    {
    return
      !value.isEmpty() && (Character.isWhitespace( value.charAt( 0)) || Character.isWhitespace( value.charAt( value.length() - 1)))
      ? String.format( "\"%s\"", value)
      : value;
    }

  private static final Pattern literalEscaped_ = Pattern.compile( "[\\\\\"]");
  }
