//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.openapi.TcasesOpenApi;
import org.cornutum.tcases.openapi.reader.OpenApiReader;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefReader;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefWriter;
import org.cornutum.tcases.resolve.ResolverConditionNotifier;
import org.cornutum.tcases.resolve.ResolverContext;

import io.swagger.v3.oas.models.OpenAPI;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.LoggerFactory;
import org.junit.runners.Parameterized;
import static org.apache.commons.lang3.StringUtils.trimToNull;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link RequestCaseResolver} applied to a {@link TcasesOpenApi#getRequestExamplesModel request examples input model}.
 */
@RunWith(Parameterized.class)
public class RequestExamplesResolverTest
  {
  @Parameters(name = "{0}")
  public static Iterable<Object[]> apiBaseNames()
    {
    return
      Arrays.stream( exampleGroups_)
      .flatMap( prefix -> getApiBaseNames( prefix))
      .filter( baseName -> !baseName.contains( "-error-"))
      .map( baseName -> new Object[]{ baseName })
      .collect( toList());
    }

  /**
   * Returns base names for all OpenApi resources with the given prefix.
   */
  protected static Stream<String> getApiBaseNames( String prefix)
    {
    return
      getApiResources( prefix)
      .map( RequestExamplesResolverTest::getApiBaseName)
      .sorted();
    }

  /**
   * Returns all OpenAPI resources with the given base name.
   */
  protected static Stream<File> getApiResources( String baseName)
    {
    String testBaseName = trimToNull( System.getProperty( "testBaseName"));

    return
      testBaseName == null?
      Arrays.stream( getResourceDir().listFiles( baseNameResources( baseName))) :

      testBaseName.startsWith( baseName)?
      Arrays.stream( getResourceDir().listFiles( baseNameResources( testBaseName))) :

      Stream.empty();      
    }

  /**
   * Returns a filter that matches all test resources with the given base name.
   */
  protected static FilenameFilter baseNameResources( String baseName)
    {
    return
      FileFilterUtils.and(
        FileFilterUtils.prefixFileFilter( baseName),
        FileFilterUtils.suffixFileFilter( apiSuffix_));
    }

  /**
   * Returns the location of the resource file for the given class.
   */
  protected static File getResourceDir()
    {
    try
      {
      return new File( RequestCaseTest.class.getResource( "..").toURI().getPath());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get resource directory path", e);
      }
    }

  /**
   * Returns the OpenAPI resource file with the given base name.
   */
  protected File getApiFile( String baseName)
    {
    return new File( getResourceDir(), baseName + apiSuffix_);
    }

  /**
   * Returns the base name of the given OpenAPI file.
   */
  protected static String getApiBaseName( File apiFile)
    {
    return apiFile.getName().replaceAll( apiSuffix_, "");
    }
  
  /**
   * Creates a new RequestExamplesResolverTest instance.
   */
  public RequestExamplesResolverTest( String apiBase)
    {
    apiBase_ = apiBase;
    }

  @Test
  public void verifyRequestCases()
    {
    // Given...
    File apiFile = getApiFile( apiBase_);

    // When...
    RequestTestDef requestTestDef;
    try
      {
      requestTestDef =
        RequestCases.getRequestCases(
          Tcases.getTests( TcasesOpenApi.getRequestExamplesModel( readApi( apiFile)), null, null),
          getResolverContext());
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get request case from file=%s", apiFile.getName()), e);
      }

    // Then...
    verifyRequestCases( apiFile, requestTestDef);
    }

  /**
   * Returns the {@link ResolverContext} for this test.
   */
  protected ResolverContext getResolverContext()
    {
    return
      ResolverContext.builder( random_)
      .notifier(
        "fail".equals( System.getProperty( "testNotifier"))
        ? ResolverConditionNotifier.fail()
        : ResolverConditionNotifier.log( LoggerFactory.getLogger( RequestCaseResolver.class)))
      .build();
    }

  /**
   * Verifies that request cases defined from the given OpenAPI file match expectations.
   */
  private void verifyRequestCases( File apiFile, RequestTestDef requestTestDef)
    {
    String baseName = getApiBaseName( apiFile);
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

  /**
   * Returns the {@link OpenAPI} object represented by the given document resource.
   */
  private OpenAPI readApi( File apiFile)
    {
    try( OpenApiReader reader = new OpenApiReader( apiFile))
      {
      return reader.read();
      }
    }

  private String apiBase_;

  private Random random_ = new Random( seed_);

  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);

  private static long seed_ =
    Optional.ofNullable( System.getProperty( "seed"))
    .map( Long::valueOf)
    .orElse( 6745393444958854970L);

  private static final String[] exampleGroups_ = new String[] {
    "example"
  };

  private static final String apiSuffix_ = ".json";
  }
