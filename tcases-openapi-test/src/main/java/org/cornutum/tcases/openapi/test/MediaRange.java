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
     * Return the "type" from this media range.
     */
    public String type()
      {
      return type_;
      }

    /**
     * Return the "subtype" from this media range.
     */
    public String subtype()
      {
      return subtype_;
      }

    /**
     * Return the "suffix" from this media range.
     */
    public String suffix()
      {
      return suffix_;
      }

    /**
     * Return the "parameter" from this media range.
     */
    public String parameter()
      {
      return parameter_;
      }

    @Override
    public String toString()
      {
      return
        String.format(
          "%s/%s%s%s",
          type_,
          subtype_,
          Optional.ofNullable( suffix_).map( suffix -> String.format( "+%s", suffix)).orElse( ""),
          Optional.ofNullable( parameter_).orElse( ""));          
      }
    
    private final String type_;
    private final String subtype_;
    private final String suffix_;
    private final String parameter_;
    
    private static final Pattern mediaRange_ = Pattern.compile( "([^\\s/]+)/([^\\s+;]+)(?:\\+([^\\s;]+))?((?:;[^\\s;=]+=[^\\s;=]+)+)?");
    }