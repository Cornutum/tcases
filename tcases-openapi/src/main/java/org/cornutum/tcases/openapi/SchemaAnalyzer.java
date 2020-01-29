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
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
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
      getValidTypes( api, schema);

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
  private Set<String> getValidTypes( OpenAPI api, Schema<?> schema)
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
        .mapToObj( i -> new SimpleEntry<Integer,Set<String>>( i, resultFor( String.format( "allOf[%s]", i), () -> getValidTypes( api, allOfMembers.get(i)))))
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
        .mapToObj( i -> resultFor( String.format( "anyOf[%s]", i), () -> getValidTypes( api, anyOfMembers.get(i))))
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
        .mapToObj( i -> resultFor( String.format( "oneOf[%s]", i), () -> getValidTypes( api, oneOfMembers.get(i))))
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

  }
