//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;
import io.swagger.v3.oas.models.OpenAPI;

/**
 * Runs tests for {@link SchemaAnalyzer}.
 */
public class SchemaAnalyzerTest extends OpenApiTest
  {
  @Test
  public void whenParameterUnsatisfiable()
    {
    // Given...
    OpenAPI api = readApi( "unsatisfiableParam");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing UnsatisfiableParam, /request, GET, param0",
      "This schema can't be satisfied by any instance");

    assertWarnings(
      "UnsatisfiableParam,/request,GET,param0,allOf: This combination of schemas can't be satisfied.",
      "UnsatisfiableParam,/request,GET,param0: This schema can't be satisfied by any instance of types=[integer].");
    }
  
  @Test
  public void whenParameterContentUnsatisfiable()
    {
    // Given...
    OpenAPI api = readApi( "unsatisfiableParamContent");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing UnsatisfiableParamContent, /request, GET, param0",
      "This schema can't be satisfied by any instance");

    assertWarnings(
      "UnsatisfiableParamContent,/request,GET,param0,oneOf: oneOf[1] can't be satisfied exclusively -- ignoring this schema.",
      "UnsatisfiableParamContent,/request,GET,param0: This schema can't be satisfied by any instance of types=[number, string, object].");
    }
  
  @Test
  public void whenRequestBodyUnsatisfiable()
    {
    // Given...
    OpenAPI api = readApi( "unsatisfiableRequestBody");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing UnsatisfiableRequestBody, /request, GET, requestBody, application/json",
      "This schema can't be satisfied by any instance");

    assertWarnings(
      "UnsatisfiableRequestBody,/request,GET,requestBody,application/json,alpha: This schema can't be satisfied by any instance of types=[number].",
      "UnsatisfiableRequestBody,/request,GET,requestBody,application/json: This schema can't be satisfied by any instance of types=[object].");
    }
  
  @Test
  public void whenResponseBodyUnsatisfiable()
    {
    // Given...
    OpenAPI api = readApi( "unsatisfiableResponseBody");

    // Then...
    assertOpenApiException(
      () -> getResponseInputModel( api),
      "Error processing UnsatisfiableResponseBody, /request, GET, 200, content, application/json",
      "This schema can't be satisfied by any instance");

    assertWarnings(
      "UnsatisfiableResponseBody,/request,GET,200,content,application/json: This schema can't be satisfied by any instance of types=[object].");
    }
  
  @Test
  public void whenHeaderUnsatisfiable()
    {
    // Given...
    OpenAPI api = readApi( "unsatisfiableHeader");

    // Then...
    assertOpenApiException(
      () -> getResponseInputModel( api),
      "Error processing UnsatisfiableHeader, /request, GET, 200, headers, x-my-secret-header",
      "This schema can't be satisfied by any instance");

    assertWarnings(
      "UnsatisfiableHeader,/request,GET,200,headers,x-my-secret-header,items: This schema can't be satisfied by any instance of types=null.",
      "UnsatisfiableHeader,/request,GET,200,headers,x-my-secret-header: This schema can't be satisfied by any instance of types=[array].");
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  protected ModelOptions getModelOptions()
    {
    return withConditionRecorder();
    }
  }
