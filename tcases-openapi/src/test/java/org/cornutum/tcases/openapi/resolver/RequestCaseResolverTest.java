//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
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

/**
 * Runs tests for {@link RequestCaseResolver}.
 */
@RunWith(Parameterized.class)
public class RequestCaseResolverTest extends RequestCaseTest
  {
  @Parameters(name = "{0}")
  public static Iterable<Object[]> testDefBaseNames()
    {
    return getTestDefBaseNameParams();
    }
  
  /**
   * Creates a new RequestCaseResolverTest instance.
   */
  public RequestCaseResolverTest( String testDefBase)
    {
    testDefBase_ = testDefBase;
    }

  @Test
  public void verifyRequestCases()
    {
    // Given...
    RequestCaseDefiner definer = new RequestCaseDefiner();
    RequestCaseResolver resolver = new RequestCaseResolver( getResolverContext());
    File testDefFile = getTestDefFile( testDefBase_);

    // When...
    try
      {
      SystemTestDef testDef = SystemTestResource.of( testDefFile).getSystemTestDef();
      getRequestCaseDefs( definer, testDef).stream()
        .map( requestCaseDef -> resolver.resolve( requestCaseDef))
        .forEach( requestCase -> {
          System.out.println( String.format( "%s: %s", testDefFile.getName(), requestCase));
          });
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get request case from file=%s", testDefFile.getName()), e);
      }
    }

  /**
   * Returns the {@link ResolverContext} for this test.
   */
  protected ResolverContext getResolverContext()
    {
    return
      ResolverContext.builder( getRandom())
      .notifier(
        "fail".equals( System.getProperty( "testNotifier"))
        ? ResolverConditionNotifier.fail()
        : ResolverConditionNotifier.log())
      .build();
    }

  private String testDefBase_;
  }
