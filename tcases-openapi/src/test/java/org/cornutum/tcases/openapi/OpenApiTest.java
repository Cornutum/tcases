//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.hamcrest.ExpectedFailure.Failable;
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
import static org.apache.commons.lang3.StringUtils.trimToNull;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * Base class for tests that verify Tcases models derived from Open API documents.
 */
public abstract class OpenApiTest
  {
  /**
   * Verifies the expected request input model for the given API.
   */
  protected void verifyRequestInputModel( String apiName)
    {
    verifyRequestInputModel( apiName, apiName);
    }
  
  /**
   * Verifies the expected request input model for the given API.
   */
  protected void verifyRequestInputModel( String apiName, String expectedName)
    {
    verifyInputModel( apiName, expectedName, api -> getRequestInputModel( api));
    }
  
  /**
   * Verifies the expected request examples model for the given API.
   */
  protected void verifyRequestExamplesModel( String apiName)
    {
    verifyRequestExamplesModel( apiName, apiName);
    }
  
  /**
   * Verifies the expected request examples model for the given API.
   */
  protected void verifyRequestExamplesModel( String apiName, String expectedName)
    {
    verifyInputModel( apiName, expectedName, api -> getRequestExamplesModel( api));
    }
  
  /**
   * Verifies the expected response input model for the given API.
   */
  protected void verifyResponseInputModel( String apiName)
    {
    verifyResponseInputModel( apiName, apiName);
    }
  
  /**
   * Verifies the expected response input model for the given API.
   */
  protected void verifyResponseInputModel( String apiName, String expectedName)
    {
    verifyInputModel( apiName, expectedName, api -> getResponseInputModel( api));
    }

  /**
   * Returns a request input model for the given API.
   */
  protected SystemInputDef getRequestInputModel( OpenAPI api)
    {
    SystemInputDef inputDef = TcasesOpenApi.getRequestInputModel( api, getModelOptions());
    return inputDef;
    }

  /**
   * Returns a response input model for the given API.
   */
  protected SystemInputDef getResponseInputModel( OpenAPI api)
    {
    SystemInputDef inputDef = TcasesOpenApi.getResponseInputModel( api, getModelOptions());
    return inputDef;
    }

  /**
   * Returns a request examples model for the given API.
   */
  protected SystemInputDef getRequestExamplesModel( OpenAPI api)
    {
    SystemInputDef inputDef = TcasesOpenApi.getRequestExamplesModel( api, getModelOptions());
    return inputDef;
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  protected ModelOptions getModelOptions()
    {
    // By default, use default options.
    return null;
    }

  /**
   * Verifies the expected input model for the given API.
   */
  protected void verifyInputModel( String apiName, String expectedName, Function<OpenAPI,SystemInputDef> inputDefSupplier)
    {
    // Given...
    SystemInputDef inputDef = verifiedInputModel( apiName, expectedName, inputDefSupplier);

    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);

    // Then...
    if( acceptAsExpected())
      {
      updateExpectedTestDef( expectedName, testDef);
      }
    else
      {
      SystemTestDef expectedTestDef = readExpectedTestDef( expectedName);
      assertThat( apiName + " test cases", testDef, matches( new SystemTestDefMatcher( expectedTestDef)));
      }
    }

  /**
   * Verifies and returns the expected input model for the given API.
   */
  protected SystemInputDef verifiedInputModel( String apiName, String expectedName, Function<OpenAPI,SystemInputDef> inputDefSupplier)
    {
    // Given...
    OpenAPI api = readApi( apiName);

    // When...
    SystemInputDef inputDef = inputDefSupplier.apply( api);

    // Then...
    if( acceptAsExpected())
      {
      updateExpectedInputDef( expectedName, inputDef);
      }
    else
      {
      SystemInputDef expectedInputDef = readExpectedInputDef( expectedName);
      assertThat( apiName + " input model", inputDef, matches( new SystemInputDefMatcher( expectedInputDef)));
      }

    return inputDef;
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
    URL url = null;
    InputStream document = null;
    List<String> docTypes = Arrays.asList( "json", "yaml", "yml"); 
    for( int i = 0; document == null && i < docTypes.size(); i++)
      {
      String resourceFile = String.format( "%s.%s", resource, docTypes.get(i));
      url = getClass().getResource( resourceFile);
      document = getClass().getResourceAsStream( resourceFile);
      }
    assertThat( "OpenAPI spec for resource=" + resource, document, is( notNullValue()));
    
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
    return readInputDef( expectedFor( resource));
    }

  /**
   * Returns the {@link SystemInputDef} object represented by the given document resource.
   */
  protected SystemInputDef readInputDef( String resource)
    {
    return inputResources_.read( inputDefFor( resource));
    }

  /**
   * Updates the given expected {@link SystemInputDef} resource.
   */
  protected void updateExpectedInputDef( String resource, SystemInputDef inputDef)
    {
    updateInputDef( expectedFor( resource), inputDef);
    }

  /**
   * Updates the given {@link SystemInputDef} resource.
   */
  protected void updateInputDef( String resource, SystemInputDef inputDef)
    {
    inputResources_.write( inputDef, new File( saveExpectedDir_, inputDefFor( resource)));
    }

  /**
   * Returns the expected {@link SystemTestDef} object represented by the given document resource.
   */
  protected SystemTestDef readExpectedTestDef( String resource)
    {
    return readTestDef( expectedFor( resource));
    }

  /**
   * Returns the {@link SystemTestDef} object represented by the given document resource.
   */
  protected SystemTestDef readTestDef( String resource)
    {
    return testResources_.read( testDefFor( resource));
    }

  /**
   * Updates the given expected {@link SystemTestDef} resource.
   */
  protected void updateExpectedTestDef( String resource, SystemTestDef testDef)
    {
    updateTestDef( expectedFor( resource), testDef);
    }

  /**
   * Updates the given {@link SystemTestDef} resource.
   */
  protected void updateTestDef( String resource, SystemTestDef testDef)
    {
    testResources_.write( testDef, new File( saveExpectedDir_, testDefFor( resource)));
    }

  /**
   * Returns the name of the given {@link SystemInputDef} resource.
   */
  protected String inputDefFor( String resource)
    {
    return resource + "-Input.xml";
    }

  /**
   * Returns the name of the given {@link SystemTestDef} resource.
   */
  protected String testDefFor( String resource)
    {
    return resource + "-Test.xml";
    }

  /**
   * Returns the name of the given expected result resource.
   */
  protected String expectedFor( String resource)
    {
    return resource + "-Expected";
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
    assertOpenApiException(
      () -> TcasesOpenApi.getRequestInputModel( readApi( apiName)),
      expected);
    }

  /**
   * Verifies that a valid examples input model can't be created for the given API spec for the specified reasons.
   */
  protected void assertRequestExamplesModelFailure( String apiName, String... expected)
    {
    assertOpenApiException(
      () -> TcasesOpenApi.getRequestExamplesModel( readApi( apiName)),
      expected);
    }

  /**
   * Verifies that an OpenApiException occurs when the given Failable is executed.
   */
  protected void assertOpenApiException( Failable failable, String... expected)
    {
    expectFailure( OpenApiException.class)
      .when( failable)
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat( "Causes", causes.build().collect( toList()), listsMembers( expected));
        });
    }

  /**
   * Returns true if all generated test results are automatically accepted.
   */
  private boolean acceptAsExpected()
    {
    return saveExpectedDir_ != null;
    }

  /**
   * Returns {@link ModelOptions} to record conditions notified.
   */
  protected ModelOptions withConditionRecorder()
    {
    conditionRecorder_ =  new ModelConditionRecorder();
    return ModelOptions.builder().notifier( conditionRecorder_).build();
    }

  protected ModelConditionRecorder getConditionRecorder()
    {
    return conditionRecorder_;
    }
  
  protected void assertWarnings( String... warnings)
    {
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( warnings));
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( emptyList()));
    }

  protected void assertErrors( String... errors)
    {
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( errors));
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( emptyList()));
    }
  
  protected void assertConditions( List<String> warnings, List<String> errors)
    {
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( warnings));
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( errors));
    }
  
  private final SystemInputResources inputResources_ = new SystemInputResources( getClass());
  private final SystemTestResources testResources_ = new SystemTestResources( getClass());

  private final File saveExpectedDir_ =
    Optional.ofNullable( trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);

  private ModelConditionRecorder conditionRecorder_;
  }
