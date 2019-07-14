//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

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
    return has( name, toStream( values));
    }

  /**
   * Adds an annotation using a comma-separated list of the string representations of the given values.
   */
  public T has( String name, Stream<?> values)
    {
    return
      hasIf(
        name,

        values
        .map( value -> String.valueOf( value))
        .reduce( (list,value) -> list + "," + value));
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
  }
