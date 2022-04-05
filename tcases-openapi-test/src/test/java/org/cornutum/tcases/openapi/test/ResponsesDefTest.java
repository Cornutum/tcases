//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.FileReader;

/**
 * Runs tests for {@link ResponsesDef}.
 */
public class ResponsesDefTest
  {
  @Test
  public void responses_0()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-0");

    // Then...
    assertThat( "Defined", responses.defined( "DELETE", "/responses", 200), is( true));
    assertThat( "Has body", responses.hasBody( "DELETE", "/responses", 200), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "DELETE", "/responses", 200, "application/json"), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "DELETE", "/responses", 200, "application/xml"), is( false));
    assertThat( "Schema defined", responses.contentSchema( "DELETE", "/responses", 200, "application/json").isPresent(), is( true));
    assertThat( "Defined", responses.defined( "DELETE", "/responses", 500), is( true));
    }

  @Test
  public void responses_1()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-1");

    // Then...
    assertThat( "Defined", responses.defined( "get", "/responses", 200), is( true));
    assertThat( "Has body", responses.hasBody( "get", "/responses", 200), is( false));
    assertThat( "Defined", responses.defined( "get", "/responses", 500), is( false));

    assertThat( "Defined", responses.defined( "head", "/responses", 201), is( true));
    assertThat( "Has body", responses.hasBody( "head", "/responses", 201), is( false));
    assertThat( "Defined", responses.defined( "head", "/responses", 500), is( false));

    assertThat( "Defined", responses.defined( "options", "/responses", 202), is( true));
    assertThat( "Has body", responses.hasBody( "options", "/responses", 202), is( false));
    assertThat( "Defined", responses.defined( "options", "/responses", 500), is( false));
    }

  @Test
  public void responses_2()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-2");

    // Then...
    assertThat( "Defined", responses.defined( "patch", "/responses", 200), is( true));
    assertThat( "Has body", responses.hasBody( "patch", "/responses", 200), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "patch", "/responses", 200, "text/javascript"), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "patch", "/responses", 200, "application/xml"), is( false));
    assertThat( "Schema defined", responses.contentSchema( "patch", "/responses", 200, "text/javascript").isPresent(), is( true));

    assertThat( "Defined", responses.defined( "patch", "/responses", 404), is( true));
    assertThat( "Has body", responses.hasBody( "patch", "/responses", 404), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "patch", "/responses", 404, "application/json"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "patch", "/responses", 404, "application/json").isPresent(), is( true));

    assertThat( "Defined", responses.defined( "patch", "/responses", 500), is( false));
    }

  @Test
  public void responses_3()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-3");

    // Then...
    assertThat( "Defined", responses.defined( "POST", "/responses", 200), is( true));
    assertThat( "Has body", responses.hasBody( "POST", "/responses", 200), is( false));
    assertThat( "Defined", responses.defined( "POST", "/responses", 500), is( true));

    expectFailure( IllegalArgumentException.class)
      .when( () -> responses.defined( "POST", "/undefined", 200))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "POST /undefined: no OpenAPI response definitions found"));
        });
    expectFailure( IllegalArgumentException.class)
      .when( () -> responses.defined( "get", "/responses", 200))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "get /responses: no OpenAPI response definitions found"));
        });
    }

  @Test
  public void responses_4()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-4");

    // Then...
    assertThat( "Defined", responses.defined( "PUT", "/responses", 200), is( true));
    assertThat( "Has body", responses.hasBody( "PUT", "/responses", 200), is( false));

    assertThat( "Defined", responses.defined( "PUT", "/responses", 405), is( true));
    assertThat( "Has body", responses.hasBody( "PUT", "/responses", 405), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "PUT", "/responses", 405, "text/plain"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "PUT", "/responses", 405, "text/plain").isPresent(), is( false));
    assertThat( "Content type defined", responses.contentTypeDefined( "PUT", "/responses", 405, "text/xml"), is( false));
    assertThat( "Schema defined", responses.contentSchema( "PUT", "/responses", 405, "text/xml").isPresent(), is( false));

    assertThat( "Defined", responses.defined( "PUT", "/responses", 500), is( true));
    assertThat( "Has body", responses.hasBody( "PUT", "/responses", 500), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "PUT", "/responses", 500, "text/plain"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "PUT", "/responses", 500, "text/plain").isPresent(), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "PUT", "/responses", 500, "text/xml"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "PUT", "/responses", 500, "text/xml").isPresent(), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "PUT", "/responses", 500, "application/json"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "PUT", "/responses", 500, "application/json").isPresent(), is( false));
    }

  @Test
  public void responses_5()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-5");

    // Then...
    assertThat( "Defined", responses.defined( "trace", "/responses", 201), is( true));
    assertThat( "Has body", responses.hasBody( "trace", "/responses", 201), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "trace", "/responses", 201, "application/json"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "trace", "/responses", 201, "application/json").isPresent(), is( true));
    assertThat( "Content type defined", responses.contentTypeDefined( "trace", "/responses", 201, "text/javascript"), is( true));
    assertThat( "Schema defined", responses.contentSchema( "trace", "/responses", 201, "text/javascript").isPresent(), is( true));

    assertThat( "Defined", responses.defined( "trace", "/responses", 500), is( false));
    }

  @Test
  public void responses_6()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-6");

    // Then...
    assertThat( "Defined", responses.defined( "HEAD", "/responses", 200), is( true));
    assertThat( "Has body", responses.hasBody( "HEAD", "/responses", 200), is( false));
    }

  @Test
  public void forPaths()
    {
    // Given...
    ResponsesDef responses = readResponses( "responsesDef-1");
    ResponsesDef view;

    // Then...
    assertThat( "Paths", responses.paths(), listsMembers( "/responses", "/respond","/respondingly"));
    assertThat( "/responses ops", responses.ops( "/responses"), listsMembers( "get", "head","options"));
    
    // When....
    view = responses.forPaths().forOps();
    
    // Then...
    assertThat( "New view", view != responses, is( true));
    assertThat( "Unchanged view", view, is( responses));

    // When....
    view = responses.forPaths( "/responses").forOps( "get");
    
    // Then...
    assertThat( "New view", view != responses, is( true));
    assertThat( "Paths", view.paths(), listsMembers( "/responses"));
    assertThat( "/responses ops", view.ops( "/responses"), listsMembers( "get"));
    assertThat( "Unchanged get /responses", view.opDef( "get", "/responses") == responses.opDef( "get", "/responses"), is( true));

    // When....
    view = responses.forOps( "head");
    
    // Then...
    assertThat( "New view", view != responses, is( true));
    assertThat( "Paths", view.paths(), listsMembers( responses.paths()));
    assertThat( "/responses ops", view.ops( "/responses"), listsMembers( "head"));
    assertThat( "/respondingly ops", view.ops( "/respondingly"), listsMembers( "head"));
    assertThat( "/respond ops", view.ops( "/respond"), is( empty()));
    }

  /**
   * Reads response definitions from a resource file.
   */
  private ResponsesDef readResponses( String resource)
    {
    try( FileReader reader = new FileReader( getClass().getResource( resource + ".json").getFile()))
      {
      return ResponsesDef.read( reader);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read ResponsesDef", e);
      }
    }
  }
