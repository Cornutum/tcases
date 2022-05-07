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
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/writeOnly", 200, "application/json");

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
    locations
      .forEach( location -> {
        assertThat( location + " property defined", !schemaWithoutWriteOnly.at( location).isMissingNode(), is( true));
        assertThat( location + " property required", isRequired( schemaWithoutWriteOnly, location), is( false));
        });
    }

  @Test
  public void schemaWriteOnlyArray()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/writeOnly", 201, "application/json");

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
    locations
      .forEach( location -> {
        assertThat( location + " property defined", !schemaWithoutWriteOnly.at( location).isMissingNode(), is( true));
        assertThat( location + " property required", isRequired( schemaWithoutWriteOnly, location), is( false));
        });
    }

  @Test
  public void schemaWriteOnlyNone()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/writeOnly", 400, "application/json");

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
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/object_1", 200, "application/json");
    List<JsonPointer> schemaLocations = ResponseAnalyzer.schemaWriteOnly( schema);

    {
    // When...
    JsonNode content = toJson( "{}");
    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);
      
    // Then...
    assertThat( "Locations", contentLocations, is( empty()));
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

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "{\"A\":[{\"id\":0}]}"));
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

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "{\"A\":[{\"id\":201951564},{\"id\":201951564}],\"wgnlflrbhlv\":{},\"mgsfx\":{\"AP2\":\"\"}}"));
    }
    }

  @Test
  public void contentWriteOnlyObject_2()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/object_2", 200, "application/json");
    List<JsonPointer> schemaLocations = ResponseAnalyzer.schemaWriteOnly( schema);

    {
    // When...
    JsonNode content = toJson(
      "{"
      + "  \"AW\": ["
      + "    {"
      + "      \"id\": 544627845,"
      + "      \"value\": -4287465815440130600"
      + "    },"
      + "    {}"
      + "  ],"
      + "  \"vdd\": {"
      + "    \"AP1\": \"\""
      + "  },"
      + "  \"w\": {"
      + "    \"AP1\": \"?\","
      + "    \"AP2\": \"??\""
      + "  },"
      + "  \"kbhqob\": {}"
      + "}");

    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);

    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "{\"vdd\":{},\"w\":{\"AP2\":\"??\"},\"kbhqob\":{}}"));
    }
    }

  @Test
  public void contentWriteOnlyObject_3()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/object_3", 200, "application/json");
    List<JsonPointer> schemaLocations = ResponseAnalyzer.schemaWriteOnly( schema);

    {
    // When...
    JsonNode content = toJson(
      "{"
      + "  \"O\": {"
      + "    \"Q\": \"(}Tn[\","
      + "    \"K\": -4025866082958271000,"
      + "    \"qlmatldlbgnmlcfz\": {},"
      + "    \"ehbhmjwf\": {}"
      + "  },"
      + "  \"ahuuzljs\": 821"
      + "}");

    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);

    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "{\"O\":{\"qlmatldlbgnmlcfz\":{},\"ehbhmjwf\":{}},\"ahuuzljs\":821}"));
    }
    {
    // When...
    JsonNode content = toJson(
      "{"
      + "  \"O\": {"
      + "    \"Q\": -4242234795304463000,"
      + "    \"msknkca\": {"
      + "      \"W\": -3706223701876914700,"
      + "      \"pt\": \"U\","
      + "      \"hyhz\": ["
      + "        \"\""
      + "      ]"
      + "    }"
      + "  }"
      + "}");

    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);

    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "{\"O\":{\"msknkca\":{\"pt\":\"U\",\"hyhz\":[\"\"]}}}"));
    }
    }

  @Test
  public void contentWriteOnlyArray()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-writeOnly");
    ObjectNode schema = expectResponseBodySchema( responses, "get", "/array", 200, "application/json");
    List<JsonPointer> schemaLocations = ResponseAnalyzer.schemaWriteOnly( schema);

    {
    // When...
    JsonNode content = toJson( "[]");

    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);

    // Then...
    assertThat( "Locations", contentLocations, is( empty()));
    }

    {
    // When...
    JsonNode content = toJson(
      "["
      + "  {"
      + "    \"X\": -738884913,"
      + "    \"W0\": -860194096,"
      + "    \"Z\": {"
      + "      \"rwxvksae\": -221,"
      + "      \"zlyjkzo\": \"vEC\""
      + "    },"
      + "    \"W1\": -12577651"
      + "  }"
      + "]");

    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);

    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "[{\"X\":-738884913,\"Z\":{\"rwxvksae\":-221,\"zlyjkzo\":\"vEC\"}}]"));
    }

    {
    // When...
    JsonNode content = toJson(
      "["
      + "  {"
      + "    \"X\": true,"
      + "    \"W0\": -386477299,"
      + "    \"Z\": -539092744,"
      + "    \"W2\": -715121890"
      + "  },"
      + "  {"
      + "    \"W0\": -864774627,"
      + "    \"W1\": true,"
      + "    \"Z\": -190523888,"
      + "    \"W2\": -763518715"
      + "  }"
      + "]");

    List<JsonPointer> contentLocations = ResponseAnalyzer.contentWriteOnly( content, schemaLocations);

    // Then...
    contentLocations
      .forEach( location -> {
        assertThat( location + " property defined", !content.at( location).isMissingNode(), is( true));
        });

    // Then...
    assertThat(
      "Without writeOnly",
      String.valueOf( ResponseAnalyzer.contentWithoutWriteOnly( content, contentLocations)),
      is( "[{\"X\":true,\"Z\":-539092744},{\"Z\":-190523888}]"));
    }
    }
  }
