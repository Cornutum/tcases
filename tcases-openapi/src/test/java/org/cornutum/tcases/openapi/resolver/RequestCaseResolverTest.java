//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.io.SystemTestResource;
import org.cornutum.tcases.openapi.resolver.io.RequestCaseReader;
import org.cornutum.tcases.openapi.resolver.io.RequestCaseWriter;

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
import java.util.List;
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
    List<RequestCase> requestCases;
    try
      {
      requestCases =
        RequestCases.getRequestCases(
          SystemTestResource.of( testDefFile).getSystemTestDef(),
          getResolverContext());
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get request case from file=%s", testDefFile.getName()), e);
      }

    // Then...
    verifyRequestCases( testDefFile, requestCases);
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

  /**
   * Verifies that request cases defined from the given system test definition match expectations.
   */
  private void verifyRequestCases( File testDefFile, List<RequestCase> requestCases)
    {
    String baseName = getTestDefBaseName( testDefFile);
    if( acceptAsExpected())
      {
      updateRequestCases( baseName, requestCases);
      }
    else
      {
      assertThat( baseName, requestCases, listsMembers( RequestCaseMatcher::new, readRequestCases( baseName)));
      }
    }

  /**
   * Returns the {@link RequestCase} objects represented by the given document resource.
   */
  protected List<RequestCase> readRequestCases( String baseName)
    {
    InputStream document = getClass().getResourceAsStream( String.format( "%s-Request-Cases.json", baseName));
    assertThat( "Request cases for resource=" + baseName, document, is( notNullValue()));
    
    try( RequestCaseReader reader = new RequestCaseReader( document))
      {
      return reader.getRequestCases();
      }
    }

  /**
   * Updates the given {@link RequestCase} resource.
   */
  private void updateRequestCases( String baseName, List<RequestCase> requestCases)
    {
    File requestCaseFile = new File( saveExpectedDir_, requestCasesFor( baseName));
    try( RequestCaseWriter writer = new RequestCaseWriter( new FileOutputStream( requestCaseFile)))
      {
      writer.write( requestCases);
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
