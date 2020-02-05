//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.collections4.multimap.AbstractSetValuedMap;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toSet;

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
  public Set<Schema<?>> getAlternatives( String type)
    {
    return
      Optional.ofNullable( alternatives_.get( String.valueOf( type)))
      .orElse( emptySet()) ;
    }

  /**
   * Returns the alternatives that can be combined with schemas of the given type.
   */
  public Set<Schema<?>> getCompatibleAlternatives( String type)
    {
    return
      type == null
      ? getAlternatives()
      : Stream.concat( getAlternatives( type).stream(), getAlternatives( null).stream()).collect( toSet());
    }

  /**
   * Returns the alternatives of all types.
   */
  public Set<Schema<?>> getAlternatives()
    {
    return alternatives_.values().stream().collect( toSet());
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
   * Returns the disjunctive normal form with a singe alternative.
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

  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);
    if( undefined( this))
      {
      builder.append( "Undefined");
      }
    else
      {
      getTypes().stream().forEach( type -> builder.append( type, getAlternatives( type).size()));
      }

    return builder.toString();
    }

  public static final Dnf NONEXISTENT = null;
  
  private AlternativesMap alternatives_ = new AlternativesMap();

  /**
   * Maps an instance type to the set of alternative schemas of that type.
   */
  private static class AlternativesMap extends AbstractSetValuedMap<String,Schema<?>>
    {
    /**
     * Creates a new AlternativesMap instance.
     */
    public AlternativesMap()
      {
      super( new HashMap<String,Set<Schema<?>>>());
      }

    protected Set<Schema<?>> createCollection()
      {
      return new LinkedHashSet<Schema<?>>();
      }
    }

  }
