//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.FunctionTestDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.openapi.io.TcasesOpenApiIO;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.junit.Test;

import java.util.List;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link RequestCaseDefiner}.
 */
public class RequestCaseDefinerTest
  {
  @Test
  public void sample()
    {
    // Given...
    RequestCaseDefiner definer = new RequestCaseDefiner();
    SystemTestDef testDef = getRequestTests( "../operations-1");
    
    // When...
    List<RequestCaseDef> requestCaseDefs = getRequestCaseDefs( definer, testDef);
    
    // Then...
    for( RequestCaseDef requestCaseDef : requestCaseDefs)
      {
      System.out.println( requestCaseDef);
      }
    }

  /**
   * Returns the request test cases for the given OpenAPI spec resource.
   */
  private SystemTestDef getRequestTests( String apiResource)
    {
    return
      TcasesOpenApiIO.getRequestTests(
        getClass().getResourceAsStream( apiResource + ".json"));
    }

  /**
   * Returns the request cases for the given system test definition.
   */
  private List<RequestCaseDef> getRequestCaseDefs( RequestCaseDefiner definer, SystemTestDef testDef)
    {
    return
      toStream( testDef.getFunctionTestDefs())
      .flatMap( function -> getRequestCaseDefs( definer, function).stream())
      .collect( toList());
    }

  /**
   * Returns the request cases for the given function test definition.
   */
  private List<RequestCaseDef> getRequestCaseDefs( RequestCaseDefiner definer, FunctionTestDef testDef)
    {
    return
      toStream( testDef.getTestCases())
      .map( testCase -> definer.toRequestCaseDef( testCase))
      .collect( toList());
    }

  }
