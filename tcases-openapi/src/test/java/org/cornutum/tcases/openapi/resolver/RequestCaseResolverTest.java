//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.io.SystemTestResource;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefReader;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefWriter;
import org.cornutum.tcases.resolve.ResolverContext;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Parameterized;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.Optional;

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
    File testDefFile = getTestDefFile( testDefBase_);

    // When...
    RequestTestDef requestTestDef;
    try
      {
      requestTestDef =
        RequestCases.getRequestCases(
          SystemTestResource.of( testDefFile).getSystemTestDef(),
          getResolverContext());
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get request case from file=%s", testDefFile.getName()), e);
      }

    // Then...
    verifyRequestCases( testDefFile, requestTestDef);
    }

  /**
   * Returns the {@link ResolverContext} for this test.
   */
  @Override
  protected ResolverContext getResolverContext()
    {
    return
      ResolverContext.builder()
      .random( getRandom())
      .notifier(
        "fail".equals( System.getProperty( "testNotifier"))
        ? RequestCaseConditionNotifier.fail()
        : RequestCaseConditionNotifier.log())
      .build();
    }

  /**
   * Verifies that request cases defined from the given system test definition match expectations.
   */
  private void verifyRequestCases( File testDefFile, RequestTestDef requestTestDef)
    {
    String baseName = getTestDefBaseName( testDefFile);
    if( acceptAsExpected())
      {
      updateRequestCases( baseName, requestTestDef);
      }
    else
      {
      assertThat(
        baseName,
        requestTestDef.getRequestCases(),
        listsMembers( RequestCaseMatcher::new, readRequestCases( baseName).getRequestCases()));
      }
    }

  /**
   * Returns the {@link RequestCase} objects represented by the given document resource.
   */
  protected RequestTestDef readRequestCases( String baseName)
    {
    InputStream document = getClass().getResourceAsStream( String.format( "%s-Request-Cases.json", baseName));
    assertThat( "Request cases for resource=" + baseName, document, is( notNullValue()));
    
    try( RequestTestDefReader reader = new RequestTestDefReader( document))
      {
      return reader.getRequestTestDef();
      }
    }

  /**
   * Updates the given {@link RequestCase} resource.
   */
  private void updateRequestCases( String baseName, RequestTestDef requestTestDef)
    {
    File requestCaseFile = new File( saveExpectedDir_, requestCasesFor( baseName));
    try( RequestTestDefWriter writer = new RequestTestDefWriter( new FileOutputStream( requestCaseFile)))
      {
      writer.write( requestTestDef);
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't write request cases to file=%s", requestCaseFile), e);
      }
    }

  /**
   * Returns the name of the request cases file with the given base name
   */
  private String requestCasesFor( String baseName)
    {
    return String.format( "%s-Request-Cases.json", baseName);
    }

  /**
   * Returns true if all generated results are automatically accepted.
   */
  private boolean acceptAsExpected()
    {
    return saveExpectedDir_ != null;
    }

  private String testDefBase_;

  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);
  }
