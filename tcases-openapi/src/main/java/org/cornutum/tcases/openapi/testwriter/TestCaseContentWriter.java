//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.io.DataValueConverter;
import org.cornutum.tcases.openapi.resolver.io.DataValueJson;
import org.cornutum.tcases.openapi.resolver.io.DataValueText;
import org.cornutum.tcases.util.MapBuilder;

import java.util.Arrays;
import java.util.HashMap;
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
  public DataValueConverter<String> getConverter( String mediaType)
    {
    return
      Optional.ofNullable(
        Optional.of( mediaType)
        .filter( mt -> converters_.containsKey( mt))
        .orElse( mediaTypeMatching( mediaType)))
      .map( mt -> converters_.get( mt))
      .orElse( null);
    }

  /**
   * Return the valid media type represented by the given string.
   */
  private String validMediaType( String mediaType)
    {
    return
      Optional.ofNullable( mediaType)
      .filter( mt -> mediaRangeMatcher( mt) != null)
      .orElse( "*/*");
    }

  /**
   * Returns the registered media type that most closely matches the given <CODE>mediaType</CODE>.
   */
  private String mediaTypeMatching( String mediaType)
    {
    Matcher mediaRange = mediaRangeMatcher( mediaType);
    String type = type( mediaRange);
    String subtype = subtype( mediaRange);
    if( ANY.equals( type) || ANY.equals( subtype))
      {
      throw new IllegalArgumentException( String.format( "Can't match mediaType='%s' -- no wildcards allowed", mediaType));
      }

    return
      Arrays.asList(
        String.format( "%s/%s", type, subtype),
        String.format( "%s/*", type),
        "*/*")
      .stream()
      .filter( mt -> converters_.containsKey( mt))
      .findFirst()
      .orElse( null);
    }

  /**
   * Returns the Matcher describing the given <CODE>mediaType</CODE> as a media range.
   */
  private Matcher mediaRangeMatcher( String mediaType)
    {
    return
      Optional.of( mediaRange_.matcher( mediaType))
      .filter( Matcher::matches)
      .orElseThrow( () -> new IllegalArgumentException( String.format( "'%s' is not a valid media range", mediaType)));
    }

  /**
   * Return the "type" from a "type/subtype" media range.
   */
  private String type( Matcher mediaRangeMatcher)
    {
    return mediaRangeMatcher.group(1);
    }

  /**
   * Return the "subtype" from a "type/subtype" media range.
   */
  private String subtype( Matcher mediaRangeMatcher)
    {
    return mediaRangeMatcher.group(2);
    }

  private Map<String,DataValueConverter<String>> converters_ = new HashMap<String,DataValueConverter<String>>();
  private static final Pattern mediaRange_ = Pattern.compile( "([^\\s/]+)/([^\\s/]+)(?:;.*)?");
  private static final String ANY = "*";
  }
