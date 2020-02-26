//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.io.SystemTestResource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Parameterized;

import java.io.File;
import java.util.List;

/**
 * Runs tests for {@link RequestCaseDefiner}.
 */
@RunWith(Parameterized.class)
public class RequestCaseDefinerTest extends RequestCaseTest
  {
  @Parameters(name = "{0}")
  public static Iterable<Object[]> testDefBaseNames()
    {
    return getTestDefBaseNameParams();
    }
  
  /**
   * Creates a new RequestCaseDefinerTest instance.
   */
  public RequestCaseDefinerTest( String testDefBase)
    {
    testDefBase_ = testDefBase;
    }
  
  /**
   * Generate request cases for every test resource with the given base name.
   */
  @Test
  public void getRequestCaseDefs()
    {
    // Given...
    RequestCaseDefiner definer = new RequestCaseDefiner();
    File testDefFile = getTestDefFile( testDefBase_);

    // When...
    try
      {
      SystemTestDef testDef = SystemTestResource.of( testDefFile).getSystemTestDef();
      List<RequestCaseDef> requestCaseDefs = getRequestCaseDefs( definer, testDef);

      // Then...
      for( RequestCaseDef requestCaseDef : requestCaseDefs)
        {
        System.out.println( String.format( "%s: %s", testDefFile.getName(), requestCaseDef));
        }
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get request case from file=%s", testDefFile.getName()), e);
      }
    }

  private String testDefBase_;
  }
