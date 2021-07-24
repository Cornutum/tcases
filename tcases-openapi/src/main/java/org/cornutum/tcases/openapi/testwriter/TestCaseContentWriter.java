//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.testwriter.encoder.DataValueConverter;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueJson;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueText;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.ToString;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Base class for TestCaseWriter implementations that specify how to serialize data in a specific 
 * <a href="https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.2.md#requestBodyObject">media type</a>.
 *
 */
public abstract class TestCaseContentWriter implements TestCaseWriter
  {
  /**
   * Creates a new TestCaseContentWriter instance.
   */
  protected TestCaseContentWriter()
    {
    getDefaultConverters().forEach( (mediaType,converter) -> setConverter( mediaType, converter));
    }
  
  /**
   * Returns the default converters for this test case writer.
   */
  protected Map<String,DataValueConverter<String>> getDefaultConverters()
    {
    DataValueConverter<String> textPlain = new DataValueText();

    return
      new MapBuilder<String,DataValueConverter<String>>()
      .put( "*/*", textPlain)
      .put( "text/*", textPlain)
      .put( "text/plain", textPlain)
      .put( "application/json", new DataValueJson())
      .put( "application/*+json", new DataValueJson())
      .build();
    }
  
  /**
   * Changes the serializer used for the given media type(s). The <CODE>mediaType</CODE> can be an valid
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a>, including wild cards.
   */
  public void setConverter( String mediaType, DataValueConverter<String> converter)
    {
    if( converter == null)
      {
      converters_.remove( validMediaType( mediaType));
      }
    else
      {
      converters_.put( validMediaType( mediaType), converter);
      }
    }

  /**
   * Returns the serializer used for the given media type. The <CODE>mediaType</CODE> must be a specific
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a> with no wild cards.
   * Returns the serializer registered for the media type that most closely matches the given media type.
   */
  public Optional<DataValueConverter<String>> getConverter( String mediaType)
    {
    MediaRange mediaRange = MediaRange.of( mediaType);

    List<String> alternatives = new ArrayList<String>();
    alternatives.add( mediaRange.toString());

    Optional.ofNullable( mediaRange.suffix())
      .ifPresent( suffix -> {
        alternatives.add( String.format( "%s/%s+%s", mediaRange.type(), mediaRange.subtype(), mediaRange.suffix()));
        alternatives.add( String.format( "%s/*+%s", mediaRange.type(), mediaRange.suffix()));
        });

    alternatives.add( String.format( "%s/%s", mediaRange.type(), mediaRange.subtype()));
    alternatives.add( String.format( "%s/*", mediaRange.type()));
    alternatives.add( "*/*");

    return
      alternatives.stream()
      .filter( alternative -> converters_.containsKey( alternative))
      .map( alternative -> converters_.get( alternative))
      .findFirst();
    }
  
  /**
   * Return the valid media type represented by the given string.
   */
  private String validMediaType( String mediaType)
    {
    return
      mediaType == null
      ? "*/*"
      : MediaRange.of( mediaType).toString();
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .build();
    }

  private Map<String,DataValueConverter<String>> converters_ = new HashMap<String,DataValueConverter<String>>();

  /**
   * Represents a <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media range</a> definition.
   */
  public static class MediaRange
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
  }
