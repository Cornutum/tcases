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
    }
  }
