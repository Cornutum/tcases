package org.cornutum.tcases.openapi.test;

import java.net.URLEncoder;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.joining;

/**
 * Builds an application/x-www-form-urlencoded string.
 */
public class FormUrlBuilder
  {
  /**
   * Creates a new FormUrlBuilder instance.
   */
  private FormUrlBuilder()
    {
    }

  /**
   * Returns a new FormUrlBuilder.
   */
  public static FormUrlBuilder with()
    {
    return new FormUrlBuilder();
    }

  /**
   * Adds a field value to this form.
   */
  public FormUrlBuilder field( String name, String value)
    {
    fields_.add( new SimpleEntry<String, String>( encodeUrl( name), encodeUrl( value)));
    return this;
    }

  /**
   * Returns the encoded form string.
   */
  public String build()
    {
    return
      fields_.stream()
      .map( field -> String.format( "%s=%s", field.getKey(), field.getValue()))
      .collect( joining( "&"));
    }

  /**
   * Enccodes an application/x-www-form-urlencoded string.
   */
  protected String encodeUrl( String content)
    {
    try
      {
      return
        content == null
        ? null
        : URLEncoder.encode( content, "UTF-8");
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( String.format( "Can't encode application/x-www-form-urlencoded content=%s", content), e);
      }
    }

  private List<Map.Entry<String,String>> fields_ = new ArrayList<Map.Entry<String,String>>();
  }
