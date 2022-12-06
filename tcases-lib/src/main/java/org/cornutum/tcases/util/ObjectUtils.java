//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Defines utility methods for handling value objects.
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
    return
      toObject(
        value,
        ObjectUtils::toBoolean,
        ObjectUtils::toNumber,
        ObjectUtils::toExternalString);
    }

  /**
   * Returns an object equal to the external form of the given value.
   */
  public static Object toExternalObject( Object value)
    {
    Collection<?> collection =
      Optional.ofNullable( value)
      .filter( v -> Collection.class.isAssignableFrom( v.getClass()))
      .map( v -> ((Collection<?>) v).stream().map( e -> toExternalObject( e)).collect( toList()))
      .orElse( null);

    return
      collection == null
      ? toObject( String.valueOf( value))
      : collection;
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
  private static Object toBoolean( String value)
    {
    return
      "true".equalsIgnoreCase( value)?
      Boolean.TRUE :

      "false".equalsIgnoreCase( value)?
      Boolean.FALSE :

      null;
    }

  /**
   * Returns the Number represented by the given string, or null if not a numeric value.
   */
  private static Object toNumber( String value)
    {
    Object number;

    try
      {
      number = toExternalNumber( new BigDecimal( value));
      }
    catch( Exception e)
      {
      number = null;
      }

    return number;
    }

  /**
   * Returns the external form of the given String value.
   */
  private static Object toExternalString( String value)
    {
    return
      // "null" is the external string representation of null
      "null".equals( value)
      ? null
      : value;
    }

  /**
   * Returns the Integer represented by the given value, or null if not an integer value.
   */
  private static Object toInt( BigDecimal value)
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
  private static Object toLong( BigDecimal value)
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
    }

  /**
   * Returns an object equal to the external form of the given number value.
   */
  private static Object toExternalNumber( BigDecimal number)
    {
    return
      Optional.ofNullable(
        toObject(
          number,
          ObjectUtils::toInt,
          ObjectUtils::toLong))

      .orElse( number);
    }
  }
