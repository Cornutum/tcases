//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.JsonUtils.*;
import static org.cornutum.hamcrest.Composites.*;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import java.util.Optional;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link ResponseAnalyzer}
 */
public class ResponseAnalyzerTest extends ResponseTest
  {
  @Test
  public void schemaWriteOnlyObject()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    JsonNode schema =
      responses.contentSchema( "get", "/writeOnly", 200, "application/json")
      .orElse( null);

    // When...
    List<JsonPointer> locations = ResponseAnalyzer.schemaWriteOnly( schema);
      
    // Then...
    locations
      .forEach( location -> {
        Optional<ObjectNode> propertySchema = asObject( schema.at( location));
        assertThat( location + " schema defined", propertySchema.isPresent(), is( true));
        assertThat( location + " writeOnly", propertySchema.map( s -> s.get( "writeOnly")).orElse( null), is( BooleanNode.valueOf( true)));
        });

    assertThat(
      "locations",
      locations.stream().map( String::valueOf).collect( toList()),
      containsMembers(
        "/properties/AW",
        "/properties/I",
        "/properties/OW",
        "/properties/A/items/properties/value",
        "/properties/O/properties/AW",
        "/properties/O/properties/N",
        "/properties/O/properties/A/items/properties/value",
        "/properties/O/additionalProperties/properties/Z",
        "/properties/O/oneOf/0/properties/K",
        "/properties/O/oneOf/1/allOf/1/anyOf/0/additionalProperties/properties/M",
        "/properties/O/oneOf/1/allOf/2/properties/Q",
        "/additionalProperties/properties/AP1"));

    // When...
    JsonNode schemaWithoutWriteOnly = ResponseAnalyzer.schemaWithoutWriteOnly( schema, locations);

    // Then...
    assertThat( "Locations", ResponseAnalyzer.schemaWriteOnly( schemaWithoutWriteOnly), is( empty()));
    }

  @Test
  public void schemaWriteOnlyArray()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    JsonNode schema =
      responses.contentSchema( "get", "/writeOnly", 201, "application/json")
      .orElse( null);

    // When...
    List<JsonPointer> locations = ResponseAnalyzer.schemaWriteOnly( schema);
      
    // Then...
    locations
      .forEach( location -> {
        Optional<ObjectNode> propertySchema = asObject( schema.at( location));
        assertThat( location + " schema defined", propertySchema.isPresent(), is( true));
        assertThat( location + " writeOnly", propertySchema.map( s -> s.get( "writeOnly")).orElse( null), is( BooleanNode.valueOf( true)));
        });

    assertThat(
      "locations",
      locations.stream().map( String::valueOf).collect( toList()),
      containsMembers(
        "/items/properties/W0",
        "/items/oneOf/0/properties/W1",
        "/items/oneOf/1/properties/W2"));

    // When...
    JsonNode schemaWithoutWriteOnly = ResponseAnalyzer.schemaWithoutWriteOnly( schema, locations);

    // Then...
    assertThat( "Locations", ResponseAnalyzer.schemaWriteOnly( schemaWithoutWriteOnly), is( empty()));
    }

  @Test
  public void schemaWriteOnlyNone()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    JsonNode schema =
      responses.contentSchema( "get", "/writeOnly", 400, "application/json")
      .orElse( null);

    // When...
    List<JsonPointer> locations = ResponseAnalyzer.schemaWriteOnly( schema);
      
    // Then...
    assertThat( "Locations", locations, is( empty()));
    }

  @Test
  public void contentWriteOnlyObject_1()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    JsonNode schema = responses.contentSchema( "get", "/object_1", 200, "application/json").orElse( null);
    List<JsonPointer> schemaLocations = ResponseAnalyzer.schemaWriteOnly( schema);

    {
    // When...
    JsonNode content = toJson( "{}");
    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);
      
    // Then...
    assertThat( "Locations", contentLocations, is( empty()));
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });
    }
    {
    // When...
    JsonNode content = toJson( "{\"A\": [{\"id\": 0, \"value\": -4120082543660235000}]}") ;
    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);
      
    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });
    }
    {
    // When...
    JsonNode content = toJson(
      "{"
      + "  \"A\": ["
      + "      {"
      + "          \"id\": 201951564,"
      + "          \"value\": -4162210996959620600"
      + "      },"
      + "      {"
      + "          \"id\": 201951564,"
      + "          \"value\": -4162210996959620600"
      + "      }"
      + "    ],"
      + "  \"wgnlflrbhlv\": {"
      + "      \"AP1\": \"\""
      + "    },"
      + "  \"mgsfx\": {"
      + "      \"AP2\": \"\""
      + "    }"
      + "}") ;
    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);
      
    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });
    }
    }
  }
