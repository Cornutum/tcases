//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;

import java.util.Arrays;
import java.util.Optional;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Represents the disjunctive normal form of a schema. An instance is validated by a schema
 * if and only if it is validated by one of the alternative schemas of its {@link Dnf}. In
 * general, a schema may validate instances of multiple types, each with its own set of
 * alternatives.
 */
public class Dnf
  {
  /**
   * Creates a new Dnf instance.
   */
  public Dnf()
    {
    }

  /**
   * Adds an alternative schema.
   */
  private void addAlternative( Schema<?> alternative)
    {
    alternatives_.put( String.valueOf( alternative.getType()), alternative);
    }

  /**
   * Returns the alternatives for the given type.
   */
  public List<Schema<?>> getAlternatives( String type)
    {
    return
      Optional.ofNullable( alternatives_.get( String.valueOf( type)))
      .orElse( emptyList()) ;
    }

  /**
   * Returns the alternatives that can be combined with schemas of the given type.
   */
  public List<Schema<?>> getCompatibleAlternatives( String type)
    {
    return
      type == null
      ? getAlternatives()
      : Stream.concat( getAlternatives( type).stream(), getAlternatives( null).stream()).collect( toList());
    }

  /**
   * Returns the alternatives of all types.
   */
  public List<Schema<?>> getAlternatives()
    {
    return alternatives_.values().stream().collect( toList());
    }

  /**
   * Returns the types of instances that can be validated by this schema.
   */
  public Set<String> getTypes()
    {
    return alternatives_.keySet();
    }

  /**
   * Returns true if the given schema is unsatisfiable by any instance.
   */
  public static boolean unsatisfiable( Dnf dnf)
    {
    return exists( dnf) && undefined( dnf);
    }

  /**
   * Returns false if the given schema is a non-existent result.
   */
  public static boolean exists( Dnf dnf)
    {
    return dnf != NONEXISTENT;
    }

  /**
   * Returns false if at least one alternative is defined for this schema.
   */
  public static boolean undefined( Dnf dnf)
    {
    return dnf.alternatives_.isEmpty();
    }

  /**
   * Returns true if at least one alternative is defined for this schema.
   */
  public static boolean defined( Dnf dnf)
    {
    return !undefined( dnf);
    }

  /**
   * Returns the disjunctive normal form with the given alternatives.
   */
  public static Dnf of( Schema<?>... alternatives)
    {
    return of( Arrays.stream( alternatives));
    }

  /**
   * Returns the disjunctive normal form with the given alternatives.
   */
  public static Dnf of( Iterable<Schema<?>> alternatives)
    {
    return of( toStream( alternatives));
    }

  /**
   * Returns the disjunctive normal form with the given alternatives.
   */
  public static Dnf of( Stream<Schema<?>> alternatives)
    {
    Dnf dnf = new Dnf();
    alternatives.forEach( alternative -> dnf.addAlternative( alternative));
    return dnf;
    }

  @Override
  public String toString()
    {
    StringBuilder builder = new StringBuilder();

    builder
      .append( '\n')
      .append( "Dnf").append( '[');

    if( undefined( this))
      {
      builder.append( "Undefined");
      }
    else
      {
      getTypes().stream()
        .forEach( type -> {
          builder
            .append( "\n  ").append( type).append( " [")
            .append( "\n    ").append( getAlternatives( type).stream().map( SchemaUtils::asserts).collect( joining( "\n    ")))
            .append( "\n  ]");
          });
      }

    return
      builder
      .append( ']')
      .toString();
    }

  /**
   * Designates a non-existent result for a DNF expression.
   */
  public static final Dnf NONEXISTENT = null;

  /**
   * Designates an undefined result for a DNF expression.
   */
  public static final Dnf UNDEFINED = Dnf.of();

  
  private ListValuedMap<String,Schema<?>> alternatives_ = new ArrayListValuedHashMap<String,Schema<?>>();
  }
