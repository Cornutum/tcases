//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.test.MediaRange;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueConverter;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueJson;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueText;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.ToString;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

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
   * Returns the serializer used for the given media type. The <CODE>mediaRange</CODE> must be a specific
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a> with no wild cards.
   * Returns the serializer registered for the media type that most closely matches the given media range.
   */
  public Optional<DataValueConverter<String>> getConverter( MediaRange mediaRange)
    {
    Object[] alternatives = new Object[]{
      mediaRange.toString(),
      mediaRange.baseStructured(),
      MediaRange.anyOf( mediaRange.type(), mediaRange.suffix()),
      mediaRange.base(),
      MediaRange.anyOf( mediaRange.type()),
      MediaRange.any()};

    return
      Arrays.stream( alternatives)
      .map( String::valueOf)
      .filter( alternative -> converters_.containsKey( alternative))
      .map( alternative -> converters_.get( alternative))
      .findFirst();
    }

  /**
   * Returns the serializer used for the given media type. The <CODE>mediaType</CODE> must be a specific
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a> with no wild cards.
   * Returns the serializer registered for the media type that most closely matches the given media type.
   */
  public Optional<DataValueConverter<String>> getConverter( String mediaType)
    {
    return getConverter( MediaRange.of( mediaType));
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
  }
