//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.CollectionUtils;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Base class for building {@link Annotated} instances.
 */
public abstract class AnnotatedBuilder<T extends AnnotatedBuilder<T>>
  {
  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  protected abstract Annotated getAnnotated();

  /**
   * Adds an annotation.
   */
  @SuppressWarnings("unchecked")
  public T has( String name, Object value)
    {
    getAnnotated().setAnnotation( name, Objects.toString( value, null));
    return (T) this;
    }

  /**
   * Adds an annotation using a comma-separated list of the string representations of the given values.
   */
  public T has( String name, Object... values)
    {
    return has( name, Arrays.stream( values));
    }

  /**
   * Adds an annotation using a comma-separated list of the string representations of the given values.
   */
  public T has( String name, Iterable<?> values)
    {
    return has( name, CollectionUtils.toStream( values));
    }

  /**
   * Adds an annotation using a comma-separated list of the string representations of the given values.
   */
  public T has( String name, Stream<?> values)
    {
    return hasIf( name, Optional.ofNullable( values).map( CollectionUtils::toCsv));
    }

  /**
   * If the given iterable is non-empty, adds an annotation using a comma-separated list of the string representations of its members.
   */
  public T hasIf( String name, Iterable<?> values)
    {
    return hasIf( name, CollectionUtils.toStream( values));
    }

  /**
   * If the given stream is non-empty, adds an annotation using a comma-separated list of the string representations of its members.
   */
  public T hasIf( String name, Stream<?> values)
    {
    return hasIf( name, Optional.ofNullable( values).map( CollectionUtils::toCsv).filter( csv -> !csv.isEmpty()));
    }

  /**
   * Adds an annotation if the given value is non-null
   */
  public T hasIf( String name, Object value)
    {
    return hasIf( name, Optional.ofNullable( value));
    }

  /**
   * Adds an annotation if the given value is defined
   */
  @SuppressWarnings("unchecked")
  public T hasIf( String name, Optional<?> value)
    {
    value.ifPresent( v -> has( name, v));
    return (T) this;
    }

  /**
   * Adds an annotation if the given value is non-null
   */
  @SuppressWarnings("unchecked")
  public T annotations( Annotated other)
    {
    getAnnotated().addAnnotations( other);
    return (T) this;
    }
  }
