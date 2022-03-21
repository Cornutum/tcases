//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.test.ResponsesDef;

import io.swagger.v3.oas.models.OpenAPI;
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.StringReader;
import java.io.StringWriter;

/**
 * Runs tests for {@link ResponsesDef} using
 * variations of the basic properties of an API definition.
 */
public class ResponsesDefTest extends OpenApiTest
  {
  @Test
  public void responses_0()
    {
    // Given...
    OpenAPI api = readApi( "responses-0");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  @Test
  public void responses_1()
    {
    // Given...
    OpenAPI api = readApi( "responses-1");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  @Test
  public void responses_2()
    {
    // Given...
    OpenAPI api = readApi( "responses-2");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  @Test
  public void responses_3()
    {
    // Given...
    OpenAPI api = readApi( "responses-3");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  @Test
  public void responses_4()
    {
    // Given...
    OpenAPI api = readApi( "responses-4");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  @Test
  public void responses_5()
    {
    // Given...
    OpenAPI api = readApi( "responses-5");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  @Test
  public void responses_6()
    {
    // Given...
    OpenAPI api = readApi( "responses-6");

    // When...
    ResponsesDef responses = OpenApiUtils.responsesDef( api, null, null);

    // Then...
    assertResponsesJson( responses);
    }

  /**
   * Writes the given response definitions to a string.
   */
  private String writeResponses( ResponsesDef responses)
    {
    try( StringWriter writer = new StringWriter())
      {
      ResponsesDef.write( responses, writer);
      return writer.toString();
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't write ResponsesDef", e);
      }
    }

  /**
   * Reads response definitions from a string.
   */
  private ResponsesDef readResponses( String responses)
    {
    try( StringReader reader = new StringReader( responses))
      {
      return ResponsesDef.read( reader);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read ResponsesDef", e);
      }
    }

  /**
   * Verify reading and writing JSON for the given response definitions.
   */
  private void assertResponsesJson( ResponsesDef responses)
    {
    assertThat( "From JSON", readResponses( writeResponses( responses)), is( responses));
    }
  }
