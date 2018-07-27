//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import java.math.BigDecimal;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Defines utility methods for reading and writing objects.
 *
 */
public final class ObjectUtils
  {
  private ObjectUtils()
    {
    // Static methods only
    }
  
  /**
   * Returns the object represented by the given string.
   */
  public static Object toObject( String value)
    {
    return toObject( value, toBoolean, toNumber, toString);
    }
  
  
  /**
   * Returns the first non-null object produced by the given converters.
   * Returns null if no such object found.
   */
  @SafeVarargs
  private static <T> Object toObject( T value, Function<T,Object>... converters)
    {
    return
      Stream.of( converters)
      .map( converter -> converter.apply( value))
      .filter( Objects::nonNull)
      .findFirst()
      .orElse( null);
    }

  /**
   * Returns the Boolean represented by the given string, or null if not a boolean value.
   */
  private static Function<String,Object> toBoolean =
    value ->
      "true".equalsIgnoreCase( value)?
      Boolean.TRUE :

      "false".equalsIgnoreCase( value)?
      Boolean.FALSE :

      null;

  /**
   * Returns the given String value.
   */
  private static Function<String,Object> toString =
    value ->
      // "null" is string representation of null
      "null".equals( value)
      ? null
      : value;

  /**
   * Returns the Integer represented by the given value, or null if not an integer value.
   */
  private static Function<BigDecimal,Object> toInt =
    value ->
      {
      Object number;
        
      try
        {
        number =
          value.scale() == 0
          ? Integer.valueOf( value.intValueExact())
          : null;
        }
      catch( Exception e)
        {
        number = null;
        }
        
      return number;
      };

  /**
   * Returns the Long represented by the given value, or null if not an long value.
   */
  private static Function<BigDecimal,Object> toLong =
    value ->
      {
      Object number;
        
      try
        {
        number =
          value.scale() == 0
          ? Long.valueOf( value.longValueExact())
          : null;
        }
      catch( Exception e)
        {
        number = null;
        }
        
      return number;
      };

  /**
   * Returns the given BigDecimal value.
   */
  private static Function<BigDecimal,Object> toDecimal =
    value -> value;

  /**
   * Returns the Number represented by the given string, or null if not a numeric value.
   */
  private static Function<String,Object> toNumber =
    value ->
      {
      Object number;

      try
        {
        number = toObject( new BigDecimal( value), toInt, toLong, toDecimal);
        }
      catch( Exception e)
        {
        number = null;
        }

      return number;
      };
  }
