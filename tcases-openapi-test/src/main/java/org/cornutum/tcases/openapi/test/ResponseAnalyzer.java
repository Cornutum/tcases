//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.CollectionUtils.*;
import static org.cornutum.tcases.openapi.test.JsonUtils.*;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.openapi4j.core.validation.ValidationSeverity;
import org.openapi4j.schema.validator.ValidationData;
import org.openapi4j.schema.validator.v3.SchemaValidator;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

/**
 * Analyzes the schema and content for an API response.
 */
public final class ResponseAnalyzer
  {
  /**
   * Creates a new ResponseAnalyzer instance.
   */
  private ResponseAnalyzer( JsonNode schema, List<JsonNode> content)
    {
    // Static methods only
    }

  /**
   * Returns the location of each "writeOnly" object property defined in the given schema.
   */
  public static List<JsonPointer> schemaWriteOnly( JsonNode schema)
    {
    return schemaWriteOnly( emptyList(), schema);
    }

  /**
   * Returns the location of each "writeOnly" object property defined in the given schema.
   */
  private static List<JsonPointer> schemaWriteOnly( List<String> location, JsonNode schema)
    {
    Map<Boolean,List<Map.Entry<String,JsonNode>>> propertiesByWriteOnly =
      asObject( schema.get( "properties"))
      .map( properties -> toStream( properties.fields()))
      .orElse( Stream.empty())
      .collect( groupingBy( e -> Optional.ofNullable( e.getValue().get( "writeOnly")).map( JsonNode::asBoolean).orElse( false)));

    List<JsonPointer> writeOnly =
      Optional.ofNullable( propertiesByWriteOnly.get( true)).orElse( emptyList())
      .stream()
      .map( e -> pointer( concatList( location, "properties", e.getKey())))
      .collect( toList());

    List<JsonPointer> propertiesWriteOnly =
      Optional.ofNullable( propertiesByWriteOnly.get( false)).orElse( emptyList())
      .stream()
      .flatMap( e -> schemaWriteOnly( concatList( location, "properties", e.getKey()), e.getValue()).stream())
      .collect( toList());

    List<JsonPointer> additionalPropertiesWriteOnly =
      asObject( schema.get( "additionalProperties"))
      .map( additionalProperties -> schemaWriteOnly( concatList( location, "additionalProperties"), additionalProperties))
      .orElse( emptyList());

    List<JsonPointer> itemsWriteOnly =
      asObject( schema.get( "items"))
      .map( items -> schemaWriteOnly( concatList( location, "items"), items))
      .orElse( emptyList());

    List<JsonPointer> allOfWriteOnly =
      asArray( schema.get( "allOf"))
     .map( allOf -> {
        return
          IntStream.range( 0, allOf.size())
          .mapToObj( i -> schemaWriteOnly( concatList( location, "allOf", String.valueOf( i)), allOf.get(i)))
          .flatMap( List::stream)
          .collect( toList());
        })
      .orElse( emptyList());

    List<JsonPointer> anyOfWriteOnly =
      asArray( schema.get( "anyOf"))
     .map( anyOf -> {
        return
          IntStream.range( 0, anyOf.size())
          .mapToObj( i -> schemaWriteOnly( concatList( location, "anyOf", String.valueOf( i)), anyOf.get(i)))
          .flatMap( List::stream)
          .collect( toList());
        })
      .orElse( emptyList());

    List<JsonPointer> oneOfWriteOnly =
      asArray( schema.get( "oneOf"))
     .map( oneOf -> {
        return
          IntStream.range( 0, oneOf.size())
          .mapToObj( i -> schemaWriteOnly( concatList( location, "oneOf", String.valueOf( i)), oneOf.get(i)))
          .flatMap( List::stream)
          .collect( toList());
        })
      .orElse( emptyList());

    return
      concatStream(
        writeOnly.stream(),
        propertiesWriteOnly.stream(),
        additionalPropertiesWriteOnly.stream(),
        itemsWriteOnly.stream(),
        allOfWriteOnly.stream(),
        anyOfWriteOnly.stream(),
        oneOfWriteOnly.stream())
      .collect( toList());
    }

  /**
   * Returns the location of each "writeOnly" object property defined in the given content.
   */
  public static List<JsonPointer> contentWriteOnly( JsonNode content, List<JsonPointer> schemaWriteOnly)
    {
    return
      schemaWriteOnly.stream()
      .flatMap( location -> matchesAt( content, location).stream())
      .filter( location -> !content.at( location).isMissingNode())
      .collect( toList());
    }

  /**
   * Return the given schema after removing definitions for all "writeOnly" properties.
   */
  public static JsonNode schemaWithoutWriteOnly( JsonNode schema, List<JsonPointer> schemaWriteOnly)
    {
    schemaWriteOnly
      .forEach( location -> {
        // Ensure writeOnly property not required.
        String property = tailOf( location);
        JsonPointer locationProperties = location.head();
        ObjectNode objectSchema = expectObject( schema.at( locationProperties.head()));
        if( objectSchema.has( "required"))
          {
          objectSchema.set(
            "required",
            createArrayNode()
            .addAll(
              toStream( objectSchema.get( "required").elements())
              .filter( required -> !property.equals( required.asText()))
              .collect( toList())));
          }
        });

    return schema;
    }

  /**
   * Return the given content after removing values for all "writeOnly" properties.
   */
  public static JsonNode contentWithoutWriteOnly( JsonNode content, List<JsonPointer> contentWriteOnly)
    {
    contentWriteOnly
      .forEach( location -> {
        // Remove writeOnly property value
        JsonPointer locationProperties = location.head();
        ObjectNode properties = expectObject( content.at( locationProperties));
        String property = tailOf( location);
        properties.remove( property);
        });

    return content;
    }

  /**
   * If any content alternative satisfies the schema, returns {@link Optional#empty}. Otherwise, returns
   * a list of validation errors. If <CODE>writeOnlyInvalid</CODE> is true, returns a validation
   * error for each occurrence of a "writeOnly" property.
   */
  public static Optional<List<SchemaValidationError>> validate( JsonNode schema, List<JsonNode> contentAlternatives, boolean writeOnlyInvalid) throws Exception
    {
    // Given a schema that may define "writeOnly" properties...
    List<JsonPointer> schemaWriteOnly =
      writeOnlyInvalid
      ? schemaWriteOnly( schema)
      : emptyList();

    // ...apply a schema without "writeOnly" properties...
    JsonNode schemaWithoutWriteOnly = schemaWithoutWriteOnly( schema, schemaWriteOnly);

    // ... to content alternatives without "writeOnly" properties
    Map<JsonNode,List<JsonPointer>> content =
      contentAlternatives.stream()
      .collect(
        toOrderedMap(
          json -> json,
          json -> contentWriteOnly( json, schemaWriteOnly)))
      .entrySet().stream()
      .collect(
        toOrderedMap(
          e -> contentWithoutWriteOnly( e.getKey(), e.getValue()),
          e -> e.getValue()));

    // ... collecting any validation errors
    SchemaValidator schemaValidator = new SchemaValidator( null, schemaWithoutWriteOnly);
    Map<JsonNode,List<SchemaValidationError>> contentErrors =
      content
      .entrySet().stream()
      .collect(
        toOrderedMap(
          e -> e.getKey(),
          e -> {
          ValidationData<Void> validation = new ValidationData<>();
          schemaValidator.validate( e.getKey(), validation);
          return validationErrors( validation);
        }));

    // Select the content alternative that best represents validation results from among...
    // ...those with only "writeOnly" errors...
    List<JsonNode> contentWriteOnly =
      contentErrors
      .keySet().stream()
      .filter( json -> contentErrors.get( json).isEmpty())
      .collect( toList());

    // ...those with no errors of any type...
    Optional<JsonNode> contentValid =
      contentWriteOnly.stream()
      .filter( json -> content.get( json).isEmpty())
      .findFirst();

    // ...and those with some errors of some type (fewest errors first)...
    List<JsonNode> contentInvalid =
      contentValid.isPresent()?
      emptyList() :

      contentErrors.keySet().stream()
      .sorted( (json1, json2) -> Integer.compare( contentErrors.get( json1).size(), contentErrors.get( json2).size()))
      .collect( toList());
    
    JsonNode withErrors =
      // Is there a content alternative with no errors?
      contentInvalid.isEmpty()?
      null :

      // Is there a content alternative with only "writeOnly" errors?
      !contentWriteOnly.isEmpty()?
      contentWriteOnly.get(0) :

      // Is there an invalid content alternative that has the expected type?
      contentInvalid.stream()
      .filter( json -> contentErrors.get( json).stream().noneMatch( error -> "#type".equals( error.getLocation())))
      .findFirst()

      // Otherwise, report errors for the first invalid alternative
      .orElse( contentInvalid.get(0));

    // Return all errors reported for the selected content alternative
    return
      Optional.ofNullable( withErrors)
      .map( json -> {
        return
          Stream.concat(
            contentErrors.get( json).stream(),
            writeOnlyErrors( content.get( json)).stream())
          .collect( toList());
        });
      
    }

  /**
   * Returns the locations in the given content that matches the given schema location.
   */
  private static List<JsonPointer> matchesAt( JsonNode content, JsonPointer schemaLocation)
    {
    return matchesAt( content, pathOf( schemaLocation));
    }

  /**
   * Returns the locations in the given content that matches the given schema location.
   */
  private static List<JsonPointer> matchesAt( JsonNode content, List<String> schemaPath)
    {
    int start;
    for( start = 0;

         start < schemaPath.size() - 1
           && ("allOf".equals( schemaPath.get(start)) || "anyOf".equals( schemaPath.get(start)) || "oneOf".equals( schemaPath.get(start)))
           && isInteger( schemaPath.get(start+1));

         start += 2);

    List<String> path = schemaPath.subList( start, schemaPath.size());
    return
      path.isEmpty()?
      emptyList() :

      path.get(0).equals( "items")?
      matchesItemsAt( content, path) :

      path.get(0).equals( "additionalProperties")?
      matchesAdditionalAt( content, path) :

      matchesPropertyAt( content, path);
    }

  /**
   * Returns the locations in the given content that matches the given property location.
   */
  private static List<JsonPointer> matchesPropertyAt( JsonNode content, List<String> schemaPath)
    {
    JsonPointer propertyLocation = pointer( schemaPath.get(1));
    JsonNode propertyValue = content.at( propertyLocation);
    List<String> pathFrom = schemaPath.subList( 2, schemaPath.size());

    return
      propertyValue.isMissingNode()?
      emptyList() :

      pathFrom.isEmpty()?
      singletonList( propertyLocation) :

      matchesAt( propertyValue, pathFrom)
      .stream()
      .map( location -> propertyLocation.append( location))
      .collect( toList());
    }

  /**
   * Returns the locations in the given content that matches the given array location.
   */
  private static List<JsonPointer> matchesItemsAt( JsonNode content, List<String> schemaPath)
    {
    List<String> pathFrom = schemaPath.subList( 1, schemaPath.size());

    return
      asArray( content)

      .map( array -> {
        return
          IntStream.range( 0, array.size())
          .mapToObj( i -> {
            return
              matchesAt( array.get(i), pathFrom)
              .stream()
              .map( location -> pointer( String.valueOf( i)).append( location))
              .collect( toList());
            })
          .flatMap( List::stream)
          .collect( toList());
        })
      
      .orElse( emptyList());
    }

  /**
   * Returns the locations in the given content that matches additional properties of the given object location.
   */
  private static List<JsonPointer> matchesAdditionalAt( JsonNode content, List<String> schemaPath)
    {
    List<String> pathFrom = schemaPath.subList( 1, schemaPath.size());

    return
      asObject( content)

      .map( object -> {
        return
          toStream( object.fieldNames())
          .map( property -> {
            return
              matchesAt( object.get( property), pathFrom)
              .stream()
              .map( location -> pointer( property).append( location))
              .collect( toList());
            })
          .flatMap( List::stream)
          .collect( toList());
        })
      
      .orElse( emptyList());
    }

  /**
   * Returns the errors described by the given schema validation results.
   */
  private static List<SchemaValidationError> validationErrors( ValidationData<Void> validation)
    {
    return
      validation.results().items( ValidationSeverity.ERROR).stream()
      .map( item -> {
        // Extract JSON pointer fragment for the location of the failed schema assertion
        StringBuilder schemaKeys = new StringBuilder();
        String crumbs = item.schemaCrumbs();
        int keyEnd = crumbs.lastIndexOf( '>');
        boolean moreSchemaKeys = keyEnd >= 0;

        while( moreSchemaKeys)
          {
          int keyStart = crumbs.lastIndexOf( '<', keyEnd);
          moreSchemaKeys = keyStart >= 0;
          if( moreSchemaKeys)
            {
            schemaKeys.insert( 0, crumbs.substring( keyStart + 1, keyEnd));
            keyEnd = keyStart - 2;
            }

          moreSchemaKeys =
            moreSchemaKeys
            && keyEnd > 0
            && crumbs.substring( keyEnd, keyStart).equals( ">.");
          if( moreSchemaKeys)
            {
            schemaKeys.insert( 0, "/");
            }
          }

        return new SchemaValidationError( item.dataCrumbs(), schemaKeys.toString(), item.message());
        })
      .collect( toList());
    }

  /**
   * Returns the errors described by the given schema validation results.
   */
  private static List<SchemaValidationError> writeOnlyErrors( List<JsonPointer> contentWriteOnly)
    {
    return
      contentWriteOnly.stream()
      .map( location -> location.toString().substring( 1))
      .map( dataLocation -> new SchemaValidationError( dataLocation, "writeOnly", "'writeOnly' property not allowed in response"))
      .collect( toList());
    }    
  }
