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
import static org.cornutum.tcases.util.CollectionUtils.*;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.collections4.SetUtils;
import org.cornutum.tcases.util.MapBuilder;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

/**
 * Analyzes the input space defined by an OpenAPI schema.
 */
public class SchemaAnalyzer extends ConditionReporter<OpenApiContext>
  {
  /**
   * Creates a new SchemaAnalyzer instance.
   */
  public SchemaAnalyzer( OpenApiContext context)
    {
    super( context);
    }

  /**
   * Returns a fully-analyzed version of the given schema.
   */
  public Schema<?> analyze( OpenAPI api, Schema<?> schema)
    {
    Schema<?> resolved = resolve( api, schema);
    analyzeValidTypes( api, resolved);
    if( Dnf.unsatisfiable( analyzeDnf( resolved)))
      {
      throw new IllegalStateException( "This schema can't be satisfied by any instance");
      }
    
    return resolved;
    }

  /**
   * Returns the resolved definition of the given schema.
   */
  private Schema<?> resolve( OpenAPI api, Schema<?> schema)
    {
    Schema<?> resolved = resolveSchema( api, schema);

    if( resolved != null && !hasResolvedAll( resolved))
      {
      setResolvedAll( resolved, false);

      Optional.ofNullable( resolved.getNot())
        .ifPresent( not -> resolved.setNot( resultFor( "not", () -> resolve( api, not))));
      
      Optional.ofNullable( asComposedSchema( resolved))
        .ifPresent( composed -> resolveMembers( api, composed));
    
      Optional.ofNullable( asArraySchema( resolved))
        .ifPresent( array -> resolveItems( api, array));

      resolveProperties( api, resolved);

      setResolvedAll( resolved, true);
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
    if( schema != null && !hasValidTypesAll( schema))
      {
      validTypes( api, schema);
      setValidTypesAll( schema, false);

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

      setValidTypesAll( schema, true);
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
  private Dnf analyzeDnf( Schema<?> schema)
    {
    return
      Optional.ofNullable( schema)
      .map( s -> dnfFor( s, getValidTypes( s)))
      .orElse( Dnf.NONEXISTENT);
    }

  /**
   * Returns the disjunctive normal form for the given schema that validates the given instance types.
   */
  private Dnf dnfFor( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( getDnf( schema))
      .orElseGet( () -> {

        setDnf(
          schema,
          withWarningIf(
            toDnf( schema, validTypes),
            Dnf::unsatisfiable,
            String.format( "This schema can't be satisfied by any instance of types=%s", validTypes)));

        analyzeSubDnfs( schema);
        
        return getDnf( schema);
        });
    }

  /**
   * Returns the disjunctive normal form of the given schema.
   */
  private Dnf toDnf( Schema<?> schema, Set<String> validTypes)
    {
    return
      allOf(
        leafDnf( schema),

        getNotDnf( schema, validTypes),

        resultFor(
          "allOf",
          () ->
          withWarningIf(
            allOf( getAllOfDnfs( schema, validTypes)),
            Dnf::unsatisfiable,
            "This combination of schemas can't be satisfied")),

        resultFor(
          "oneOf",
          () ->
          withWarningIf(
            oneOf( getOneOfDnfs( schema, validTypes)),
            Dnf::unsatisfiable,
            "This combination of schemas can't be satisfied")),

        resultFor(
          "anyOf",
          () ->
          withWarningIf(
            anyOf( getAnyOfDnfs( schema, validTypes)),
            Dnf::unsatisfiable,
            "This combination of schemas can't be satisfied")));
    }

  /**
   * Returns the disjunctive normal form for the negation of the given schema
   */
  private Dnf dnfForNot( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( getDnf( schema))
      .orElseGet( () -> {

        setDnf(
          schema,
          withWarningIf(
            toNotDnf( schema, validTypes),
            Dnf::unsatisfiable,
            String.format( "This schema can't be satisfied by any instance of types=%s", validTypes)));

        analyzeSubDnfs( schema);
        
        return getDnf( schema);
        });
    }

  /**
   * Returns the disjunctive normal form of the leaf assertions of the given schema.
   */
  private Dnf leafDnf( Schema<?> schema)
    {
    return
      Optional.ofNullable( leafSchemaOf( schema))
      .map( Dnf::of)
      .orElse( Dnf.NONEXISTENT);
    }

  /**
   * Determines the disjunctive normal form of any subschemas for array items or object properties.
   */
  private void analyzeSubDnfs( Schema<?> schema)
    {
    Optional<Dnf> undefinedArrayItems = 
      Optional.ofNullable( asArraySchema( schema))
      .flatMap( array -> resultFor( "items", () -> Optional.of( analyzeDnf( array.getItems())).filter( Dnf::undefined)));

    Optional<Dnf> undefinedProperties =
      Optional.ofNullable( schema.getProperties())
      .flatMap(
        properties -> properties.keySet().stream()
        .map( p -> resultFor( p, () -> analyzeDnf( toPropertySchema( schema.getProperties().get( p)))))
        .collect( toList())
        .stream()
        .filter( Dnf::undefined)
        .findFirst());

    Optional<Dnf> undefinedAdditionalProperties =
      Optional.ofNullable( additionalPropertiesSchema( schema))
      .map( ap -> resultFor( "additionalProperties", () -> analyzeDnf( toPropertySchema( ap))))
      .filter( Dnf::undefined);

    if( undefinedArrayItems.isPresent() || undefinedProperties.isPresent() || undefinedAdditionalProperties.isPresent())
      {
      // If any subschema is unsatisfiable, so is this schema
      setDnf( schema, Dnf.UNDEFINED);
      }
    }

  /**
   * Returns the disjunctive normal form for the negation of the given schema
   */
  private Dnf toNotDnf( Schema<?> schema, Set<String> validTypes)
    {
    return
      unionOf(
        not( leafDnf( schema)),

        not( getNotDnf( schema, validTypes)),

        resultFor(
          "allOf",
          () ->
          withWarningIf(
            notAllOf( getAllOfDnfs( schema, validTypes)),
            Dnf::unsatisfiable,
            "This combination of schemas can't be satisfied")),

        resultFor(
          "oneOf",
          () ->
          withWarningIf(
            noneOf( getOneOfDnfs( schema, validTypes)),
            Dnf::unsatisfiable,
            "This combination of schemas can't be satisfied")),

        resultFor(
          "anyOf",
          () ->
          withWarningIf(
            noneOf( getAnyOfDnfs( schema, validTypes)),
            Dnf::unsatisfiable,
            "This combination of schemas can't be satisfied")));
    }

  /**
   * If the given schema defines a "not" assertion, returns its disjunctive normal form.
   * Otherwise, returns {@link Dnf#NONEXISTENT}.
   */
  private Dnf getNotDnf( Schema<?> schema, Set<String> validTypes)
    {
    return
      resultFor(
        "not",
        () ->
        Optional.ofNullable( schema.getNot())
        .map( not -> dnfForNot( not, validTypes))
        .orElse( Dnf.NONEXISTENT));
    }

  /**
   * If the given schema defines an "allOf" assertion, returns the disjunctive normal form for each member schema.
   */
  private List<Dnf> getAllOfDnfs( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( asComposedSchema( schema))
      .map(
        composed ->
        combinedAllOf( composed.getAllOf()).stream()
        .map( member -> dnfFor( member, validTypes))
        .collect( toList()))
      .orElse( emptyList());
    }

  /**
   * If the given schema defines an "oneOf" assertion, returns the disjunctive normal form for each member schema.
   */
  private List<Dnf> getOneOfDnfs( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( asComposedSchema( schema))
      .map(
        composed ->
        applicableMembers( "oneOf", validTypes, composed.getOneOf())
        .map( member -> dnfFor( member, validTypes))
        .collect( toList()))
      .orElse( emptyList());
    }

  /**
   * If the given schema defines an "anyOf" assertion, returns the disjunctive normal form for each member schema.
   */
  private List<Dnf> getAnyOfDnfs( Schema<?> schema, Set<String> validTypes)
    {
    return
      Optional.ofNullable( asComposedSchema( schema))
      .map(
        composed ->
        applicableMembers( "anyOf", validTypes, composed.getAnyOf())
        .map( member -> dnfFor( member, validTypes))
        .collect( toList()))
      .orElse( emptyList());
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by all of the given schemas.
   */
  private Dnf allOf( Dnf... dnfs)
    {
    return allOf( Arrays.asList( dnfs));
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance validated
   * by all of the given schemas.
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
            List<Schema<?>> c2 = dnf2.getCompatibleAlternatives( a1.getType());
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
            withWarningIf(
              allOf( member, noneOf( restOf( dnfs, i))),
              Dnf::undefined,
              String.format( "oneOf[%s] can't be satisfied exclusively -- ignoring this schema", i));
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
   * Returns the disjunctive normal form of the schema that validates any instance <EM>not</EM> validated
   * by any of the given schemas.
   */
  private Dnf noneOf( List<Dnf> dnfs)
    {
    return
      dnfs.stream()
      .filter( Dnf::exists)
      .flatMap( dnf -> dnf.getAlternatives().stream().map( alternative -> Dnf.of( getInvalidators( alternative))))
      .reduce( this::allOf)
      .orElse( Dnf.NONEXISTENT);
    }

  /**
   * Returns the disjunctive normal form of the schema that validates any instance <EM>not</EM> validated
   * by at least one of the given schemas.
   */
  private Dnf notAllOf( List<Dnf> dnfs)
    {
    return
      Optional.of( dnfs.stream().filter( Dnf::exists).collect( toList()))
      .filter( exist -> exist.size() == 1)
      .map( this::noneOf)
      .orElse( anyOf( dnfs));
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
    Set<Integer> unpaired = IntStream.range( 0, choices.size()).mapToObj( Integer::valueOf).collect( toOrderedSet());

    // ... unless fewer than 3 inputs. If so, skip pairing to ensure that, for each input schema, the resulting DNF can contain
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
                  Dnf.UNDEFINED:

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
    return noneOf( Arrays.asList( dnf));
    }

  /**
   * Returns the disjunctive normal form of the schema that the union of all the given DNFs.
   */
  private Dnf unionOf( List<Dnf> dnfs)
    {
    return
      Optional.of( dnfs.stream().filter( Dnf::exists).collect( toList()))
      .filter( exist -> !exist.isEmpty())
      .map( exist -> Dnf.of( exist.stream().flatMap( dnf -> dnf.getAlternatives().stream())))
      .orElse( Dnf.NONEXISTENT);
    }

  /**
   * Returns the disjunctive normal form of the schema that the union of all the given DNFs.
   */
  private Dnf unionOf( Dnf... dnfs)
    {
    return unionOf( Arrays.asList( dnfs));
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
      
    // ... return the list of remaining non-leaf members...
    List<Schema> consolidated =
      memberEquivalents.stream()
      .filter( SchemaUtils::isNotLeafSchema)
      .collect( toList());

    // ... adding a single schema combining all leaf members (if any)
    IntStream.range( 0, memberEquivalents.size())
      .mapToObj( i -> new SimpleEntry<Integer,Schema>( i, memberEquivalents.get(i)))
      .filter( memberEntry -> isLeafSchema( memberEntry.getValue()))
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
   * If so, returns the equivalent schema. Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  @SuppressWarnings({ "rawtypes" })
  private Optional<Schema> combinedAllOf( ComposedSchema schema)
    {
    return
      // Does this schema define only "allOf" members?
      schema.getAllOf().isEmpty() || !schema.getAnyOf().isEmpty() || !schema.getOneOf().isEmpty() || schema.getNot() != null?
      Optional.empty() :
      
      // Yes, does its consolidated "allOf" list consist of a single member?
      Optional.of( combinedAllOf( schema.getAllOf()))
      .filter( members -> members.size() == 1)

      // ...which is a leaf schema?
      .map( members -> members.get(0))
      .filter( SchemaUtils::isLeafSchema)

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
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getInvalidators( Schema<?> schema)
    {
    String type = Optional.ofNullable( schema).map( Schema::getType).orElse( null);;

    List<Schema<?>> alternatives = 

      schema == null?
      new ArrayList<Schema<?>>() :

      "object".equals( type)?
      getObjectInvalidators( schema) :
      
      "string".equals( type)?
      getStringInvalidators( schema) :
      
      "integer".equals( type)?
      getIntegerInvalidators( schema) :
      
      "boolean".equals( type)?
      getBooleanInvalidators( schema) :

      "array".equals( type)?
      getArrayInvalidators( schema) :
      
      "number".equals( type)?
      getNumberInvalidators( schema) :

      getGenericInvalidators( schema);

    // Empty schema?
    if( alternatives.isEmpty())
      {
      // Yes, return an alternative schema that will invalidate any instance.
      alternatives.add( FALSE_SCHEMA);
      }

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given sub-schema.
   */
  private List<Schema<?>> getSubSchemaInvalidators( String context, Schema<?> schema)
    {
    return
      resultFor( context, () -> {
        return
          isLeafSchema( schema)?
          getInvalidators( schema):
          
          Optional.ofNullable( getDnf( schema)).orElse( Dnf.UNDEFINED)
          .getAlternatives().stream()
          .flatMap( s -> getInvalidators( s).stream())
          .collect( toList());
        });
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  @SuppressWarnings({ "rawtypes" })
  private List<Schema<?>> getObjectInvalidators( Schema<?> schema)
    {
    List<Schema<?>> alternatives = getGenericInvalidators( schema);

    // Not maxProperties
    Optional.ofNullable( schema.getMaxProperties())
      .map( maxProperties -> assertNot( schema, maxProperties, (s,v) -> s.setMinProperties( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minProperties
    Optional.ofNullable( schema.getMinProperties())
      .filter( minProperties -> minProperties > 0)
      .map( minProperties -> assertNot( schema, minProperties, (s,v) -> s.setMaxProperties( v - 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not properties
    Optional.ofNullable( schema.getProperties()).orElse( emptyMap())
      .forEach( (property, propertySchema) -> {

        List<String> notRequired =
          Optional.ofNullable( schema.getRequired()).map( required -> required.contains( property)).orElse( false)
          ? null
          : Arrays.asList( property);
        
        getSubSchemaInvalidators( property, propertySchema).stream()
          .map(
            notSchema ->
            assertNot(
              schema,
              (Schema) notSchema,
              (s,v) -> { s.setProperties( MapBuilder.of( property, v).build()); s.setRequired( notRequired); }))
          .forEach( s -> alternatives.add( s));
        });

    // Not required
    Optional.ofNullable( schema.getRequired()).orElse( emptyList())
      .stream()
      .map( property -> assertNot( schema, property, (s,v) -> addNotRequired( s, v)))
      .forEach( s -> alternatives.add( s));      

    // Not additionalProperties
    Optional.ofNullable( schema.getAdditionalProperties())
      .ifPresent( ap -> {
        if( ap instanceof Schema)
          {
          getSubSchemaInvalidators( "additionalProperties", (Schema<?>) ap).stream()
            .map( notSchema -> assertNot( schema, notSchema, (s,v) -> s.setAdditionalProperties( v)))
            .forEach( s -> alternatives.add( s));
          }
        else
          {
          alternatives.add( assertNot( schema, (Boolean) ap, (s,v) -> s.setAdditionalProperties( !v)));
          }
        });

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getStringInvalidators( Schema<?> schema)
    {
    List<Schema<?>> alternatives = getGenericInvalidators( schema);

    // Not maxLength
    Optional.ofNullable( schema.getMaxLength())
      .map( maxLength -> assertNot( schema, maxLength, (s,v) -> s.setMinLength( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minLength
    Optional.ofNullable( schema.getMinLength())
      .filter( minLength -> minLength > 0)
      .map( minLength -> assertNot( schema, minLength, (s,v) -> s.setMaxLength( v - 1)))
      .ifPresent( s -> alternatives.add( s));

    // Not patterns
    Optional.ofNullable( getPatterns( schema))
      .ifPresent( patterns -> {
        patterns.stream()
          .map( pattern -> assertNot( schema, pattern, (s,v) -> addNotPattern( s, v)))
          .forEach( s -> alternatives.add( s));
        });

    // Not not patterns
    Optional.ofNullable( getNotPatterns( schema))
      .ifPresent( patterns -> {
        patterns.stream()
          .map( pattern -> assertNot( schema, pattern, (s,v) -> addPattern( s, v)))
          .forEach( s -> alternatives.add( s));
        });

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getIntegerInvalidators( Schema<?> schema)
    {
    return getNumericInvalidators( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getBooleanInvalidators( Schema<?> schema)
    {
    return getGenericInvalidators( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getArrayInvalidators( Schema<?> schema)
    {
    List<Schema<?>> alternatives = getGenericInvalidators( schema);

    // Not maxItems
    Optional.ofNullable( schema.getMinItems())
      .filter( max -> max > 0)
      .map( max -> assertNot( schema, max, (s,v) -> s.setMaxItems( v - 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minItems
    Optional.ofNullable( schema.getMaxItems())
      .map( min -> assertNot( schema, min, (s,v) -> s.setMinItems( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not uniqueItems
    Optional.ofNullable( schema.getUniqueItems())
      .map( unique -> assertNot( schema, unique, (s,v) -> s.setUniqueItems( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not items
    Optional.ofNullable( asArraySchema( schema))
      .ifPresent(
        array -> 
        Optional.ofNullable( array.getItems())
          .ifPresent(
            items ->
            getSubSchemaInvalidators( "items", items).stream()
            .map( notItems -> assertNot( array, notItems, (s,v) -> s.setItems( notItems)))
            .forEach( s -> alternatives.add( s))));
    
    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getNumberInvalidators( Schema<?> schema)
    {
    return getNumericInvalidators( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getNumericInvalidators( Schema<?> schema)
    {
    List<Schema<?>> alternatives = getGenericInvalidators( schema);

    // Not maximum
    boolean exclusiveMaximum = !Boolean.TRUE.equals( schema.getExclusiveMinimum());
    Optional.ofNullable( schema.getMinimum())
      .map( max -> assertNot( schema, max, (s,v) -> s.setMaximum( max)))
      .ifPresent( s -> { s.setExclusiveMaximum( exclusiveMaximum); alternatives.add( s); });
    
    // Not minimum
    boolean exclusiveMinimum = !Boolean.TRUE.equals( schema.getExclusiveMaximum());
    Optional.ofNullable( schema.getMaximum())
      .map( min -> assertNot( schema, min, (s,v) -> s.setMinimum( min)))
      .ifPresent( s -> { s.setExclusiveMinimum( exclusiveMinimum); alternatives.add( s); });

    // Not exclusive maximum
    Optional.ofNullable( schema.getExclusiveMaximum())
      .map( exclusive -> assertNot( schema, exclusive, (s,v) -> s.setExclusiveMaximum( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not exclusive minimum
    Optional.ofNullable( schema.getExclusiveMinimum())
      .map( exclusive -> assertNot( schema, exclusive, (s,v) -> s.setExclusiveMinimum( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not multipleOf
    Optional.ofNullable( schema.getMultipleOf())
      .map( multipleOf -> assertNot( schema, multipleOf, (s,v) -> addNotMultipleOf( s, v)))
      .ifPresent( s -> alternatives.add( s));

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private List<Schema<?>> getGenericInvalidators( Schema<?> schema)
    {
    List<Schema<?>> alternatives = new ArrayList<Schema<?>>();

    // Not type
    Optional.ofNullable( schema.getType())
      .map( type -> assertNot( EMPTY_SCHEMA, type, (s,v) -> addNotType( s, v)))
      .ifPresent( s -> alternatives.add( s));

    // Not nullable
    Optional.ofNullable( schema.getNullable())
      .map( nullable -> assertNot( schema, nullable, (s,v) -> s.setNullable( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not enums
    Optional.ofNullable( schema.getEnum())
      .filter( enums -> !isExampleSchema( schema))
      .map( enums -> assertNot( schema, enums, (s,v) -> setNotEnums( s, v)))
      .ifPresent( s -> alternatives.add( s));

    // Not readOnly
    Optional.ofNullable( schema.getReadOnly())
      .map( readOnly -> assertNot( schema, readOnly, (s,v) -> s.setReadOnly( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not writeOnly
    Optional.ofNullable( schema.getWriteOnly())
      .map( writeOnly -> assertNot( schema, writeOnly, (s,v) -> s.setWriteOnly( !v)))
      .ifPresent( s -> alternatives.add( s));
    
    return alternatives;
    }

  /**
   * Returns an alternate schema that negates the assertion of the given value from the orginal schema.
   */
  @SuppressWarnings("unchecked")
  private <S extends Schema<?>,V> Schema<?> assertNot( S original, V asserted, BiConsumer<S,V> negater)
    {
    Class<S> schemaType = (Class<S>) original.getClass();
    try
      {
      // Create new empty alternative schema
      S alternative = schemaType.getDeclaredConstructor().newInstance();
      alternative.setType( original.getType());
      setPropertySchema( alternative, isPropertySchema( original));

      // Remove any additional format assertion automatically added by the Schema subclass.
      alternative.setFormat( null);

      // Apply negater to add the negated assertion.
      negater.accept( alternative, asserted);

      return alternative;
      }
    catch( Exception e)
      {
      throw new RuntimeException( String.format( "Can't create alternative schema of type=%s", schemaType.getSimpleName()), e);
      }
    }

  /**
   * Returns a new leaf schema containing only the leaf assertions of the given schema.
   * Returns null if the given schema is a {@link #isNotLeafSchema non-leaf schema} with no
   * leaf assertions.
   */
  private Schema<?> leafSchemaOf( Schema<?> schema)
    {
    return
      Optional.of( copySchema( schema))
      .filter( leafSchema -> !isEmpty( leafSchema) || isLeafSchema( schema))
      .map( this::normalize)
      .orElse( null);
    }

  /**
   * Returns this schema after normalizing context-dependent assertions.
   */
  private Schema<?> normalize( Schema<?> schema)
    {
    if( !isPropertySchema( schema))
      {
      // Ignore assertions that apply only to object properties.
      schema.setReadOnly( null);
      schema.setWriteOnly( null);
      }
    
    return schema;
    }

  /**
   * Returns the given result, notifying a warning if the given predicate is true.
   */
  private <T> T withWarningIf( T result, Predicate<T> isWarning, String reason)
    {
    if( isWarning.test( result))
      {
      notifyWarning( reason);
      }
    return result;
    }
  }
