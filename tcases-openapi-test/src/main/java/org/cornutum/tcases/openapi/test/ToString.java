//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import java.util.Optional;
import java.util.StringJoiner;

/**
 * Builds a string representation for instances of a specified class.
 */
public class ToString
  {
  /**
   * Creates a new ToString instance.
   */
  private ToString( Class<?> classObj)
    {
    this( classObj.getSimpleName());
    }
  
  /**
   * Creates a new ToString instance.
   */
  private ToString( String className)
    {
    builder_ = new StringBuilder( className).append( "[");
    joiner_ = new StringJoiner( ",");
    }

  /**
   * Returns a ToString builder for the given class.
   */
  public static ToString builder( Class<?> type)
    {
    return new ToString( type);
    }

  /**
   * Adds a non-null element to the result string.
   */
  public <T> ToString add( T element)
    {
    return addIf( Optional.ofNullable( element));
    }

  /**
   * Adds an element to the result string if it is present.
   */
  public <T> ToString addIf( Optional<T> element)
    {
    element.ifPresent( e -> joiner_.add( String.valueOf( e)));
    return this;
    }

  /**
   * Adds a named element to the result string.
   */
  public <T> ToString add( String name, T element)
    {
    return addIf( name, Optional.of( String.valueOf( element)));
    }

  /**
   * Adds a named element to the result string if it is present.
   */
  public <T> ToString addIf( String name, Optional<T> element)
    {
    element.ifPresent( e -> joiner_.add( String.format( "%s=%s", name, String.valueOf( e))));
    return this;
    }

  /**
   * Returns the string produced by this builder.
   */
  @Override
  public String toString()
    {
    return
      result_ == null
      ? result_ = builder_.append( joiner_.toString()).append( "]").toString()
      : result_;
    }

  private final StringBuilder builder_;
  private final StringJoiner joiner_;
  private String result_;
  }
