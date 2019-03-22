//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.SystemTestDefMatcher;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.io.SystemTestResources;
import org.cornutum.tcases.openapi.reader.OpenApiReader;
import org.cornutum.tcases.openapi.reader.OpenApiReaderException;
import static org.cornutum.tcases.util.CollectionUtils.membersOf;

import io.swagger.v3.oas.models.OpenAPI;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.InputStream;
import java.net.URL;
import java.util.Arrays;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Base class for tests that verify Tcases models derived from Open API documents.
 */
public abstract class OpenApiTest
  {
  /**
   * Verifies expected request input model for the given API.
   */
  protected void verifyRequestInputModel( String apiName)
    {
    // Given...
    OpenAPI api = readApi( apiName);

    // When...
    SystemInputDef inputDef = TcasesOpenApi.getRequestInputModel( api);

    // Then...
    SystemInputDef expectedInputDef = readExpectedInputDef( apiName);
    assertThat( apiName + " input model", inputDef, matches( new SystemInputDefMatcher( expectedInputDef)));

    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);

    // Then...
    SystemTestDef expectedTestDef = readExpectedTestDef( apiName);
    assertThat( apiName + " test cases", testDef, matches( new SystemTestDefMatcher( expectedTestDef)));
    }

  /**
   * Verifies no request input model created for the given API.
   */
  protected void verifyRequestInputModelNone( String apiName)
    {
    // Given...
    OpenAPI api = readApi( apiName);

    // When...
    SystemInputDef inputDef = TcasesOpenApi.getRequestInputModel( api);

    // Then...
    assertThat( apiName + " input model", inputDef, is( nullValue()));
    }

  /**
   * Returns the {@link OpenAPI} object represented by the given document resource.
   */
  protected OpenAPI readApi( String resource)
    {
    String resourceFile = resource + ".json";
    URL url = getClass().getResource( resourceFile);
    InputStream document = getClass().getResourceAsStream( resourceFile);
    assertThat( "Resource=" + resourceFile, document, is( notNullValue()));
    
    try( OpenApiReader reader = new OpenApiReader( document, url))
      {
      return reader.read();
      }
    }

  /**
   * Returns the expected {@link SystemInputDef} object represented by the given document resource.
   */
  protected SystemInputDef readExpectedInputDef( String resource)
    {
    return readInputDef( resource + "-Expected");
    }

  /**
   * Returns the {@link SystemInputDef} object represented by the given document resource.
   */
  protected SystemInputDef readInputDef( String resource)
    {
    return inputResources_.read( resource + "-Input.xml");
    }

  /**
   * Returns the expected {@link SystemTestDef} object represented by the given document resource.
   */
  protected SystemTestDef readExpectedTestDef( String resource)
    {
    return readTestDef( resource + "-Expected");
    }

  /**
   * Returns the {@link SystemTestDef} object represented by the given document resource.
   */
  protected SystemTestDef readTestDef( String resource)
    {
    return testResources_.read( resource + "-Test.xml");
    }

  /**
   * Verifies that the given API spec contains the specified Open API conformance errors.
   */
  protected void assertOpenApiFailure( String apiName, String... expected)
    {
    expectFailure( OpenApiReaderException.class)
      .when( () -> readApi( apiName))
      .then( failure -> {
        assertThat
          ( "Errors",
            membersOf( failure.getErrors()).collect( toList()),
            containsInAnyOrder( Arrays.stream( expected).map( m -> containsString(m)).collect( toList())));
        });
    }

  /**
   * Verifies that a valid input model can't be created for the given API spec for the specified reasons.
   */
  protected void assertRequestInputModelFailure( String apiName, String... expected)
    {
    expectFailure( OpenApiException.class)
      .when( () -> TcasesOpenApi.getRequestInputModel( readApi( apiName)))
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat( "Causes", causes.build().collect( toList()), listsMembers( expected));
        });
    }

  private final SystemInputResources inputResources_ = new SystemInputResources( getClass());
  private final SystemTestResources testResources_ = new SystemTestResources( getClass());
  }
