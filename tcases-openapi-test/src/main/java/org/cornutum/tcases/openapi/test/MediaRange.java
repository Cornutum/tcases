package org.cornutum.tcases.openapi.test;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toMap;

/**
 * Represents a <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media range</a> definition.
 */
public class MediaRange
  {
  /**
   * Creates a new MediaRange instance.
   */
  public static MediaRange of( String text)
    {
    return new MediaRange( text);
    }

  /**
   * Creates a new MediaRange representing any subtype of the given type.
   */
  public static MediaRange anyOf( String type)
    {
    return anyOf( type, null);
    }

  /**
   * Creates a new MediaRange representing any subtype of the given type with the given suffix.
   */
  public static MediaRange anyOf( String type, String suffix)
    {
    return new MediaRange( type, "*", suffix);
    }

  /**
   * Creates a new MediaRange representing any data.
   */
  public static MediaRange any()
    {
    return anyOf( "*");
      }

  /**
   * Creates a new MediaRange instance.
   */
  private MediaRange( String text)
    {
    Matcher matcher =
      Optional.of( mediaRange_.matcher( text))
      .filter( Matcher::matches)
      .orElseThrow( () -> new IllegalArgumentException( String.format( "'%s' is not a valid media range", text)));

    type_ = matcher.group(1);
    subtype_ = matcher.group(2);
    suffix_ = matcher.group(3);

    parameters_ =
      Optional.ofNullable( matcher.group(4))
      .map( params -> Arrays.stream( params.split( ";", 0)))
      .orElse( Stream.empty())
      .map( param -> param.split( "=", -1))
      .filter( param -> param.length == 2)
      .collect(
        toMap(
          param -> param[0].trim(),
          param -> param[1].trim(),
          (v1,v2) -> v1,
          LinkedHashMap::new));
    }

  /**
   * Creates a new MediaRange instance.
   */
  private MediaRange( String type, String subtype, String suffix)
    {
    this( baseType( type, subtype, suffix));
    }
    
  /**
   * Returns the "type" from this media range.
   */
  public String type()
    {
    return type_;
    }

  /**
   * Returns the "subtype" from this media range.
   */
  public String subtype()
    {
    return subtype_;
    }

  /**
   * Returns the base type (i.e. "type/subtype" only) from this media range.
   */
  public String base()
    {
    return baseType( type(), subtype(), null);
    }

  /**
   * Returns the base type with optional suffix (i.e. "type/subtype+suffix" only) from this media range.
   */
  public String baseStructured()
    {
    return baseType( type(), subtype(), suffix());
    }

  /**
   * Returns the "suffix" from this media range.
   */
  public String suffix()
    {
    return suffix_;
    }

  /**
   * Returns the "parameter" definitions from this media range.
   */
  public Map<String,String> parameters()
    {
    return
      parameters_.entrySet().stream()
      .collect( toMap( entry -> entry.getKey(), entry -> quotedValueOf( entry.getValue())));
    }

  /**
   * Returns a base type representation.
   */
  private static String baseType( String type, String subtype, String suffix)
    {
    return
      String.format(
        "%s/%s%s",
        type,
        subtype,
        suffix == null? "" : String.format( "+%s", suffix));
    }

  /**
   * Returns the value of a (possibly quoted) string.
   */
  private static String quotedValueOf( String value)
    {
    String unquoted;

    if( !value.isEmpty() && value.indexOf( '"') == 0)
      {
      Matcher unquote = quotedChar_.matcher( value.substring( 1, Math.max( value.length() - 1, 1)));
      StringBuffer result = new StringBuffer();
      while( unquote.find())
        {
        unquote.appendReplacement( result, "$1");
        }
      unquote.appendTail( result);
      unquoted = result.toString();
      }
    else
      {
      unquoted = value;
      }
    
    return unquoted;
    }

  @Override
  public String toString()
    {
    return
      baseType( type_, subtype_, suffix_)
      +
      Optional.ofNullable( parameters_)
      .map( params -> params.entrySet().stream().map( param -> String.format( "; %s=%s", param.getKey(), param.getValue())).collect( joining()))
      .orElse( "");          
    }
    
  private final String type_;
  private final String subtype_;
  private final String suffix_;
  private final Map<String,String> parameters_;

  private static final String DELIM = "\\h()<>@,;:\\\\\"/\\[\\]?={}";
  private static final String TOKEN = String.format( "[^%s]+", DELIM);
  private static final String STOKEN = String.format( "[^%s\\+]+", DELIM);
  private static final String QSTRING = "\"(?:[^\\\"]|\\\\.)*\"";
  private static final String LWS = "\\h*";

  private static final String MEDIA_RANGE =
    String.format(
      "%s(%s)/(%s)(?:\\+(%s))?((?:%s;%s%s%s=%s(?:%s|%s)%s)+)?",
      LWS, TOKEN, STOKEN, STOKEN, LWS, LWS, TOKEN, LWS, LWS, TOKEN, QSTRING, LWS);
  
  private static final Pattern mediaRange_ = Pattern.compile( MEDIA_RANGE);
  private static final Pattern quotedChar_ = Pattern.compile( "\\\\(.)");
  }
