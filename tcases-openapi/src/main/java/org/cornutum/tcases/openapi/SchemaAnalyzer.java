//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.OpenApiUtils.*;
import static org.cornutum.tcases.openapi.SchemaExtensions.*;
import static org.cornutum.tcases.openapi.SchemaUtils.*;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.collections4.SetUtils;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

/**
 * Analyzes the input space defined by an OpenAPI schema.
 */
public class SchemaAnalyzer extends ModelConditionReporter
  {
  /**
   * Creates a new SchemaAnalyzer instance.
   */
  public SchemaAnalyzer( NotificationContext context)
    {
    setContext( context);
    }

  /**
   * Returns a fully-analyzed version of the given schema.
   */
  public Schema<?> analyze( OpenAPI api, Schema<?> schema)
    {
    Schema<?> resolved = resolve( api, schema);

    analyzeValidTypes( api, resolved);
    analyzeDnf( resolved);
    
    return resolved;
    }

  /**
   * Returns the resolved definition of the given schema.
   */
  private Schema<?> resolve( OpenAPI api, Schema<?> schema)
    {
    Schema<?> resolved = resolveSchema( api, schema);

    if( resolved != null)
      {
      Optional.ofNullable( resolved.getNot())
        .ifPresent( not -> resolved.setNot( resultFor( "not", () -> resolve( api, not))));
      
      Optional.ofNullable( asComposedSchema( resolved))
        .ifPresent( composed -> resolveMembers( api, composed));
    
      Optional.ofNullable( asArraySchema( resolved))
        .ifPresent( array -> resolveItems( api, array));

      resolveProperties( api, resolved);
      }
    
    return resolved;
    }

  /**
   * Resolves the members of a ComposedSchema.
   */
  private void resolveMembers( OpenAPI api, ComposedSchema composed)
    {
    // Resolve "allOf" schemas
    composed.setAllOf(
      Optional.ofNullable( composed.getAllOf()).orElse( emptyList())
      .stream()
      .map( member -> resultFor( "allOf", () -> resolve( api, member)))
      .collect( toList()));
      
    // Resolve "anyOf" schemas
    composed.setAnyOf(
      Optional.ofNullable( composed.getAnyOf()).orElse( emptyList())
      .stream()
      .map( member -> resultFor( "anyOf", () -> resolve( api, member)))
      .collect( toList()));
      
    // Resolve "oneOf" schemas
    composed.setOneOf(
      Optional.ofNullable( composed.getOneOf()).orElse( emptyList())
      .stream()
      .map( member -> resultFor( "oneOf", () -> resolve( api, member)))
      .collect( toList()));
    }

  /**
   * Resolves the items schema for an array schema.
   */
  private void resolveItems( OpenAPI api, ArraySchema array)
    {
    array.setItems( resultFor( "items", () -> resolve( api, array.getItems())));
    }

  /**
   * Resolves the schemas for the properties of an object schema.
   */
  private void resolveProperties( OpenAPI api, Schema<?> object)
    {
    Optional.ofNullable( object.getProperties())
      .ifPresent( properties -> {
        properties.keySet().stream()
          .forEach( p -> properties.put( p, resultFor( p, () -> resolve( api, properties.get( p)))));
        });

    Optional.ofNullable( object.getAdditionalProperties())
      .filter( additional -> additional instanceof Schema)
      .ifPresent( additional -> {
        object.setAdditionalProperties(
          resultFor( "additionalProperties", () -> resolve( api, (Schema<?>) additional)));
        });
    }

  /**
   * Determines the instance types that can be validated by the given schema.
   */
  private void analyzeValidTypes( OpenAPI api, Schema<?> schema)
    {
    if( schema != null)
      {
      validTypes( api, schema);

      Optional.ofNullable( schema.getNot())
        .ifPresent( not -> doFor( "not", () -> analyzeValidTypes( api, not)));
    
      Optional.ofNullable( asArraySchema( schema))
        .ifPresent( array -> doFor( "items", () -> analyzeValidTypes( api, array.getItems())));

      Optional.ofNullable( schema.getProperties())
        .ifPresent( properties -> {
          properties.keySet().stream()
            .forEach( p -> doFor( p, () -> analyzeValidTypes( api, properties.get( p))));
          });

      Optional.ofNullable( schema.getAdditionalProperties())
        .filter( additional -> additional instanceof Schema)
        .ifPresent( additional -> doFor( "additionalProperties", () -> analyzeValidTypes( api, (Schema<?>) additional)));

      }
    }

  /**
   * Returns the instance types that can be validated by the given schema. Returns null if any type can be validated.
   */
  private Set<String> validTypes( OpenAPI api, Schema<?> schema)
    {
    if( !hasValidTypes( schema))
      {
      setValidTypes( schema, findValidTypes( api, schema));
      }

    return SchemaExtensions.getValidTypes( schema);
    }

  /**
   * Returns the instance types that can be validated by the given schema. Returns null if any type can be validated.
   */
  @SuppressWarnings("rawtypes")
  private Set<String> findValidTypes( OpenAPI api, Schema<?> schema)
    {
    // By default, only the type declared for this schema is valid.
    Set<String> validTypes =
      Optional.ofNullable( schema.getType())
      .map( type -> Stream.of( type).collect( toSet()))
      .orElse( null);

    ComposedSchema composedSchema = asComposedSchema( schema);
    if( composedSchema != null)
      {
      resolveSchemaMembers( api, composedSchema);
      
      // If "allOf" specified, valid types may include only those accepted by all members.
      List<Schema> allOfMembers = composedSchema.getAllOf();
      Set<String> allOfTypes =
        IntStream.range( 0, allOfMembers.size())
        .mapToObj( i -> new SimpleEntry<Integer,Set<String>>( i, resultFor( String.format( "allOf[%s]", i), () -> validTypes( api, allOfMembers.get(i)))))
        .filter( memberTypes -> memberTypes.getValue() != null)
        .reduce(
          (allTypes, memberTypes) ->
          resultFor( String.format( "allOf[%s]", memberTypes.getKey()),
            () -> {
              Set<String> allAccepted = SetUtils.intersection( allTypes.getValue(), memberTypes.getValue()).toSet();
              if( allAccepted.isEmpty())
                {
                throw
                  new IllegalStateException(
                    String.format( "Valid types=%s for this member are not accepted by other \"allOf\" members", memberTypes.getValue()));
                }
              allTypes.setValue( allAccepted);
              return allTypes;
            }))
        .map( allTypes -> allTypes.getValue())
        .orElse( null);

      // Valid types include only those accepted by "allOf" and the rest of this schema.
      if( validTypes == null)
        {
        validTypes = allOfTypes;
        }
      else if( allOfTypes != null)
        {
        if( SetUtils.intersection( validTypes, allOfTypes).isEmpty())
          {
          throw new IllegalStateException( String.format( "\"allOf\" members accept types=%s but not required types=%s", allOfTypes, validTypes));
          }
        validTypes.retainAll( allOfTypes);
        }
        
      // If "anyOf" specified, valid types may include any accepted by any member.
      List<Schema> anyOfMembers = composedSchema.getAnyOf();
      Set<String> anyOfTypes =
        IntStream.range( 0, anyOfMembers.size())
        .mapToObj( i -> resultFor( String.format( "anyOf[%s]", i), () -> validTypes( api, anyOfMembers.get(i))))
        .filter( Objects::nonNull)
        .flatMap( Set::stream)
        .collect( toSet());

      // Valid types include only those accepted by "anyOf" and the rest of this schema.
      if( !anyOfTypes.isEmpty())
        {
        if( validTypes == null)
          {
          validTypes = anyOfTypes;
          }
        else if( SetUtils.intersection( validTypes, anyOfTypes).isEmpty())
          {
          throw new IllegalStateException( String.format( "\"anyOf\" members accept types=%s but not types=%s", anyOfTypes, validTypes));
          }
        else
          {
          validTypes.retainAll( anyOfTypes);
          }
        }
        
      // If "oneOf" specified, valid types may include any accepted by any member.
      List<Schema> oneOfMembers = composedSchema.getOneOf();
      Set<String> oneOfTypes =
        IntStream.range( 0, oneOfMembers.size())
        .mapToObj( i -> resultFor( String.format( "oneOf[%s]", i), () -> validTypes( api, oneOfMembers.get(i))))
        .filter( Objects::nonNull)
        .flatMap( Set::stream)
        .collect( toSet());

      // Valid types include only those accepted by "oneOf" and the rest of this schema.
      if( !oneOfTypes.isEmpty())
        {
        if( validTypes == null)
          {
          validTypes = oneOfTypes;
          }
        else if( SetUtils.intersection( validTypes, oneOfTypes).isEmpty())
          {
          throw new IllegalStateException( String.format( "\"oneOf\" members accept types=%s but not types=%s", oneOfTypes, validTypes));
          }
        else
          {
          validTypes.retainAll( oneOfTypes);
          }
        }
      }

    return validTypes;
    }

  /**
   * Returns the subset of the given member schemas that represent inputs that are applicable when only instances of the given
   * types are valid.
   */
  @SuppressWarnings("rawtypes")
  private Stream<Schema<?>> applicableMembers( String containerType, Set<String> validTypes, List<Schema> memberSchemas)
    {
    return
      IntStream.range( 0, memberSchemas.size())
      .filter(
        i -> 
        resultFor( String.format( "%s[%s]", containerType, i),
          () -> {
            boolean applicable = isApplicableInput( memberSchemas.get(i), validTypes);
            if( !applicable)
              {
              notifyWarning( String.format( "Ignoring this schema -- not applicable when only instance types=%s can be valid", validTypes));
              }
            return applicable;
          }))
      .mapToObj( i -> memberSchemas.get(i));
    }

  /**
   * Returns true if and only if the given schema represents an input that is applicable when
   * only instances of the given types are valid.
   */
  private boolean isApplicableInput( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( validTypes)
      .flatMap( required -> Optional.ofNullable( getValidTypes( schema)).map( valid -> !SetUtils.intersection( valid, required).isEmpty()))
      .orElse( true);
    }

  /**
   * Determines the disjunctive normal form of the given schema.
   */
  private void analyzeDnf( Schema<?> schema)
    {
    if( schema != null)
      {
      analyzeDnf( schema, getValidTypes( schema));
      }
    }

  /**
   * Determines the disjunctive normal form of the given schema.
   */
  private void analyzeDnf( Schema<?> schema, Set<String> validTypes)
    {
    dnfFor( schema, validTypes);
    
    Optional.ofNullable( asArraySchema( schema))
      .ifPresent( array -> doFor( "items", () -> analyzeDnf( array.getItems())));

    Optional.ofNullable( schema.getProperties())
      .ifPresent( properties -> {
        properties.keySet().stream()
          .forEach( p -> doFor( p, () -> analyzeDnf( properties.get( p))));
        });

    Optional.ofNullable( schema.getAdditionalProperties())
      .filter( additional -> additional instanceof Schema)
      .ifPresent( additional -> doFor( "additionalProperties", () -> analyzeDnf( (Schema<?>) additional)));
    }

  /**
   * Returns the disjunctive normal form for the given schema that validates the given instance types.
   */
  private Dnf dnfFor( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( getDnf( schema))
      .orElseGet( () -> {
        Dnf dnf = toDnf( schema, validTypes);
        setDnf( schema, dnf);
        return dnf;
        });
    }

  /**
   * Returns the disjunctive normal form of the given schema.
   */
  private Dnf toDnf( Schema<?> schema, Set<String> validTypes)
    {
    Dnf baseDnf = Dnf.of( schema);

    Dnf withNot =
      resultFor( "not",
        () ->
        Optional.ofNullable( schema.getNot())
        .map(
          not ->
          withFailureIf(
            allOf( baseDnf, not( dnfFor( not, validTypes))),
            Dnf::unsatisfiable,
            () -> new IllegalStateException( "\"not\" assertion inconsistent with base schema")))
        .orElse( baseDnf));

    return
      Optional.ofNullable( asComposedSchema( schema))
      .map(
        composed ->

        withFailureIf(
          allOf(
            withNot,

            resultFor(
              "allOf",
              () ->
              withFailureIf(         
                allOf(
                  combinedAllOf( composed.getAllOf()).stream()
                  .map( member -> dnfFor( member, validTypes))
                  .collect( toList())),
                Dnf::unsatisfiable,
                () -> new IllegalStateException( "This combination of schemas can't be satisfied"))),

            resultFor(
              "oneOf",
              () ->
              withFailureIf(         
                oneOf(
                  applicableMembers( "oneOf", validTypes, composed.getOneOf())
                  .map( member -> dnfFor( member, validTypes))
                  .collect( toList())),
                Dnf::unsatisfiable,
                () -> new IllegalStateException( "This combination of schemas can't be satisfied"))),

            resultFor(
              "anyOf",
              () ->
              withFailureIf(         
                anyOf(
                  applicableMembers( "anyOf", validTypes, composed.getAnyOf())
                  .map( member -> dnfFor( member, validTypes))
                  .collect( toList())),
                Dnf::unsatisfiable,
                () -> new IllegalStateException( "This combination of schemas can't be satisfied")))),

          Dnf::unsatisfiable,
          () -> new IllegalStateException( "Boolean schema combinations inconsistent with base schema")))
      
      .orElse( withNot);
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by both of the given schemas.
   */
  private Dnf allOf( Dnf... dnfs)
    {
    return allOf( Arrays.asList( dnfs));
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by both of the given schemas.
   */
  private Dnf allOf( List<Dnf> dnfs)
    {
    return
      dnfs.stream()
      .filter( Dnf::exists)
      .reduce(
        (dnf1, dnf2) ->

        Dnf.undefined( dnf1)?
        dnf1 :

        Dnf.undefined( dnf2)?
        dnf2 :

        Dnf.of(
          dnf1.getAlternatives().stream()
          .flatMap( a1 -> {
            Set<Schema<?>> c2 = dnf2.getCompatibleAlternatives( a1.getType());
            return
              c2.isEmpty()
              ? Stream.of( a1)
              : c2.stream().map( a2 -> combined( a1, a2)).filter( Objects::nonNull);
            })))
      .orElse( Dnf.NONEXISTENT);
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by exactly one of the given schemas.
   */
  private Dnf oneOf( List<Dnf> dnfs)
    {
    List<Dnf> oneOfOnly =
      // For all specified schema choices...
      IntStream.range( 0, dnfs.size())
      .mapToObj( i -> {
          Dnf member = dnfs.get(i);
          return
            // ... ignoring non-existent choices...
            !Dnf.exists( member)?
            Dnf.NONEXISTENT :

            // ... define a schema that validates only this choice
            withErrorIf(
              allOf( member, noneOf( restOf( dnfs, i))),
              Dnf::undefined,
              String.format( "oneOf[%s] can't be satisfied exclusively", i),
              "Ignoring this schema");
        })
      .filter( Dnf::exists)
      .collect( toList());
    
    return
      // Only non-existent choices?
      oneOfOnly.isEmpty()

      // Yes, result is also non-existent
      ? Dnf.NONEXISTENT

      // No, result is disjunction of exclusive choices
      : Dnf.of( oneOfOnly.stream().flatMap( dnf -> dnf.getAlternatives().stream()));
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by non of the given schemas.
   */
  private Dnf noneOf( List<Dnf> dnfs)
    {
    return
      dnfs.stream()
      .map( this::not)
      .reduce( this::allOf)
      .orElse( Dnf.NONEXISTENT);
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by one or more of the given schemas.
   */
  private Dnf anyOf( List<Dnf> dnfs)
    {
    // Ignoring any non-existent inputs...
    List<Dnf> choices = dnfs.stream().filter( Dnf::exists).collect( toList());

    // ... find pairs of schemas that can be combined...
    List<Dnf> pairs = new ArrayList<Dnf>();
    Set<Integer> unpaired = IntStream.range( 0, choices.size()).mapToObj( Integer::valueOf).collect( toSet());

    // ... unless fewer than 3 inputs. If so, skip pairing to ensure that, for each input schema, the resulting DNF contains
    // alternatives to validate and invalidate that schema.
    if( choices.size() >= 3)
      {
      IntStream.range( 0, choices.size())
        .forEach( i -> {
          // Has this schema already been paired?
          if( unpaired.contains(i))
            {
            // No, can another schema be combined with it?
            IntStream.range( 0, choices.size())
              .mapToObj( j -> {
                Dnf pair =
                  // When pairing with a different schema...
                  i == j?
                  Dnf.of():

                  // ... is there a schema that validates this pair but none of the others?
                  allOf(
                    choices.get(i),
                    choices.get(j),
                    noneOf( restOf( restOf( choices, Math.max( i, j)), Math.min( i, j))));

                if( Dnf.defined( pair))
                  {
                  // Yes, continue looking for pairs among other schemas.
                  unpaired.remove( i);
                  unpaired.remove( j);
                  }

                return pair;
                })
              .filter( Dnf::defined)
              .findFirst()
              .ifPresent( pair -> pairs.add( pair));
            }
          });
      }

    return
      // Any pairs found?
      pairs.isEmpty()?

      // No, "any of" is equivalent to "one of"
      oneOf( choices) :

      // Yes, include alternatives for...
      Dnf.of(
        Stream.concat(
          // ... all paired schemas...
          pairs.stream()
          .flatMap( dnf -> dnf.getAlternatives().stream()),

          // ... and "oneOf" alternatives for all unpaired schemas
          unpaired.stream()
          .map( i -> allOf( choices.get(i), noneOf( restOf( choices, i))))
          .flatMap( dnf -> dnf.getAlternatives().stream())));
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance <EM>not</EM> validated
   * by the given schema.
   */
  private Dnf not( Dnf dnf)
    {
    return
      Dnf.exists( dnf)?
      dnf.getAlternatives().stream()
      .map( alternative -> Dnf.of( SchemaUtils.not( alternative)))
      .reduce( this::allOf)
      .orElse( dnf)

      : Dnf.NONEXISTENT;
    }

  /**
   * Returns a consolidated list of "allOf" members by combining all leaf members into a single schema.
   */
  @SuppressWarnings("rawtypes")
  private List<Schema> combinedAllOf( List<Schema> members)
    {
    // After replacing any ComposedSchema members with equivalent leaf schemas...
    List<Schema> memberEquivalents =
      IntStream.range( 0, members.size())
      .mapToObj(
        i ->
        resultFor(
          String.format( "allOf[%s]", i),
          () -> Optional.ofNullable( asComposedSchema( members.get(i))).flatMap( c -> combinedAllOf( c)).orElse( members.get(i))))
      .collect( toList());
      
    // ... return the list of remaining ComposedSchema members...
    List<Schema> consolidated =
      memberEquivalents.stream()
      .map( SchemaUtils::asComposedSchema)
      .filter( Objects::nonNull)
      .collect( toList());

    // ... adding a single schema combining all leaf members (if any)
    IntStream.range( 0, memberEquivalents.size())
      .mapToObj( i -> new SimpleEntry<Integer,Schema>( i, memberEquivalents.get(i)))
      .filter( memberEntry -> asComposedSchema( memberEntry.getValue()) == null)
      .reduce(
        (combinedEntry, memberEntry) ->
        resultFor(
          String.format( "allOf[%s]", memberEntry.getKey()),
          () -> new SimpleEntry<Integer,Schema>( memberEntry.getKey(), combineSchemas( getContext(), combinedEntry.getValue(), memberEntry.getValue()))))
      .map( combinedEntry -> combinedEntry.getValue())
      .ifPresent( combined -> consolidated.add( 0, combined));

    return consolidated;
    }

  /**
   * The given ComposedSchema, if it consists solely of "allOf" members, may be equivalent to a single leaf schema.
   * If so, returns the equivalent schema. Otherwise, returns <CODE>Optional.emtpy()</CODE>.
   */
  @SuppressWarnings({ "rawtypes" })
  private Optional<Schema> combinedAllOf( ComposedSchema schema)
    {
    return
      // Does this schema define only "allOf" members?
      schema.getAllOf().isEmpty() || !schema.getAnyOf().isEmpty() || !schema.getOneOf().isEmpty()?
      Optional.empty() :
      
      // Yes, does its consolidated "allOf" list consist of a single member?
      Optional.of( combinedAllOf( schema.getAllOf()))
      .filter( members -> members.size() == 1)

      // ...which is a leaf schema?
      .map( members -> members.get(0))
      .filter( member -> asComposedSchema( member) == null)

      // If so, return the equivalent combined leaf schema
      .map( member -> combineSchemas( getContext(), schema, member));
    }

  /**
   * If the given schemas can be combined consistently, returns the combined schema.
   * Otherwise, returns null;
   */
  private Schema<?> combined( Schema<?> schema1, Schema<?> schema2)
    {
    try
      {
      return combineSchemas( getContext(), schema1, schema2);
      }
    catch( Exception e)
      {
      return null;
      }
    }

  /**
   * Returns a list of all given members except for the one at the excluded position.
   */
  private <T> List<T> restOf( List<T> members, int excluded)
    {
    return
      IntStream.range( 0, members.size())
      .filter( i -> i != excluded)
      .mapToObj( i -> members.get(i))
      .collect( toList());
    }

  /**
   * Returns the given result, reporting a failure if the given predicate is true.
   */
  private <T> T withFailureIf( T result, Predicate<T> isFailure, Supplier<RuntimeException> failure)
    {
    if( isFailure.test( result))
      {
      throw failure.get();
      }
    return result;
    }

  /**
   * Returns the given result, notifying an error if the given predicate is true.
   */
  private <T> T withErrorIf( T result, Predicate<T> isError, String reason, String resolution)
    {
    if( isError.test( result))
      {
      notifyError( reason, resolution);
      }
    return result;
    }

  }
