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
      "UnsatisfiableRequestBody,/request,GET,requestBody,application/json,alpha: This schema can't be satisfied by any instance of types=[number].");
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
      "UnsatisfiableHeader,/request,GET,200,headers,x-my-secret-header,items: This schema can't be satisfied by any instance of types=null.");
    }
  
  @Test
  public void whenCircularProperty()
    {
    // Given...
    OpenAPI api = readApi( "circular-property");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing Circular property, /tree, POST, requestBody, application/json, before",
      "Can't create an input model for a schema that references itself");
    }
  
  @Test
  public void whenCircularAdditionalProperty()
    {
    // Given...
    OpenAPI api = readApi( "circular-additional");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing Circular property, /tree, POST, requestBody, application/json, Additional",
      "Can't create an input model for a schema that references itself");
    }
  
  @Test
  public void whenCircularArray()
    {
    // Given...
    OpenAPI api = readApi( "circular-array");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing Circular array, /tree, POST, requestBody, application/json, children, items",
      "Can't create an input model for a schema that references itself");
    }
  
  @Test
  public void whenCircularCombination()
    {
    // Given...
    OpenAPI api = readApi( "circular-combination");

    // Then...
    assertOpenApiException(
      () -> getRequestInputModel( api),
      "Error processing Circular combination, /tree, POST, requestBody, application/json, child, child",
      "Can't create an input model for a schema that references itself");
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  protected ModelOptions getModelOptions()
    {
    return withConditionRecorder();
    }
  }
