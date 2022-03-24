package org.cornutum.tcases.openapi.test;

import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    parameter_ = matcher.group(4);
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
   * Returns the "parameter" from this media range.
   */
  public String parameter()
    {
    return parameter_;
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

  @Override
  public String toString()
    {
    return
      baseType( type_, subtype_, suffix_)
      + Optional.ofNullable( parameter_).orElse( "");          
    }
    
  private final String type_;
  private final String subtype_;
  private final String suffix_;
  private final String parameter_;
    
  private static final Pattern mediaRange_ = Pattern.compile( "([^\\s/]+)/([^\\s+;]+)(?:\\+([^\\s;]+))?((?:;[^\\s;=]+=[^\\s;=]+)+)?");
  }
