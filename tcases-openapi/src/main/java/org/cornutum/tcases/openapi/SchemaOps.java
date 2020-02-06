//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.openapi.SchemaExtensions.*;
import static org.cornutum.tcases.openapi.SchemaUtils.*;

import io.swagger.v3.oas.models.media.Schema;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap; 

/**
 * Defines operations for combining OpenAPI {@link Schema} objects.
 */
public final class SchemaOps
  {
  /**
   * Creates a new SchemaOps instance.
   */
  private SchemaOps()
    {
    // Static methods only
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  public static List<Schema<?>> not( Schema<?> schema)
    {
    String type = Optional.ofNullable( schema).map( Schema::getType).orElse( null);;

    return
      schema == null?
      notGeneric( emptySchema()) :

      "object".equals( type)?
      notObject( schema) :
      
      "string".equals( type)?
      notString( schema) :
      
      "integer".equals( type)?
      notInteger( schema) :
      
      "boolean".equals( type)?
      notBoolean( schema) :

      "array".equals( type)?
      notArray( schema) :
      
      "number".equals( type)?
      notNumber( schema) :

      notGeneric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  @SuppressWarnings({ "rawtypes" })
  private static List<Schema<?>> notObject( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

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

        not( propertySchema)
          .stream()
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
          not( (Schema<?>) ap).stream()
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
  private static List<Schema<?>> notString( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

    // Not maxLength
    Optional.ofNullable( schema.getMaxLength())
      .map( maxLength -> assertNot( schema, maxLength, (s,v) -> s.setMinLength( v + 1)))
      .ifPresent( s -> alternatives.add( s));
    
    // Not minLength
    Optional.ofNullable( schema.getMinLength())
      .filter( minLength -> minLength > 0)
      .map( minLength -> assertNot( schema, minLength, (s,v) -> s.setMaxLength( v - 1)))
      .ifPresent( s -> alternatives.add( s));

    // Not pattern
    Optional.ofNullable( schema.getPattern())
      .map( pattern -> assertNot( schema, pattern, (s,v) -> addNotPattern( s, v)))
      .ifPresent( s -> alternatives.add( s));

    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notInteger( Schema<?> schema)
    {
    return notNumeric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notBoolean( Schema<?> schema)
    {
    return notGeneric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notArray( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

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
            not( items).stream()
            .map( notItems -> assertNot( array, notItems, (s,v) -> s.setItems( notItems)))
            .forEach( s -> alternatives.add( s))));
    
    return alternatives;
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notNumber( Schema<?> schema)
    {
    return notNumeric( schema);
    }

  /**
   * Returns a list of alternative schemas that would validate an instance that is <EM>not</EM> validated
   * by the given schema.
   */
  private static List<Schema<?>> notNumeric( Schema<?> schema)
    {
    List<Schema<?>> alternatives = notGeneric( schema);

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
  private static List<Schema<?>> notGeneric( Schema<?> schema)
    {
    List<Schema<?>> alternatives = new ArrayList<Schema<?>>();

    // Not type
    Optional.ofNullable( schema.getType())
      .map( type -> assertNot( emptySchema(), type, (s,v) -> addNotType( s, v)))
      .ifPresent( s -> alternatives.add( s));

    // Not nullable
    Optional.ofNullable( schema.getNullable())
      .map( nullable -> assertNot( schema, nullable, (s,v) -> s.setNullable( !v)))
      .ifPresent( s -> alternatives.add( s));

    // Not enums
    Optional.ofNullable( schema.getEnum())
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

    // Empty schema?
    if( alternatives.isEmpty())
      {
      // Yes, return an alternative schema that will invalidate any instance.
      alternatives.add(
      assertNot(
        emptySchema(),
        SCHEMA_TYPES,
        (s,v) -> { setNotTypes( s, v); s.setNullable( false); }));
      }
    
    return alternatives;
    }

  /**
   * Returns an alternate schema that negates the assertion of the given value from the orginal schema.
   */
  @SuppressWarnings("unchecked")
  private static <S extends Schema<?>,V> Schema<?> assertNot( S original, V asserted, BiConsumer<S,V> negater)
    {
    Class<S> schemaType = (Class<S>) original.getClass();
    try
      {
      S alternative = schemaType.newInstance();
      alternative.setType( original.getType());
      alternative.setFormat( original.getFormat());

      negater.accept( alternative, asserted);

      return alternative;
      }
    catch( Exception e)
      {
      throw new RuntimeException( String.format( "Can't create alternative schema of type=%s", schemaType.getSimpleName()), e);
      }
    }
  }
