//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.io.RequestTestDefReader;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefWriter;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
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
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link RequestCases#realizeRequestCases}.
 */
@RunWith(Parameterized.class)
public class RealizeTest extends RequestCaseTest
  {
  @Parameters(name = "{0}")
  public static Iterable<Object[]> requestCases()
    {
    return getRequestCaseParams( new String[]{ "petstore-requests", "realize"});
    }

  private static Iterable<Object[]> getRequestCaseParams( String[] groups)
    {
    return
      Arrays.stream( groups)
      .flatMap( prefix -> getRequestCaseBaseNames( prefix))
      .map( baseName -> new Object[]{ baseName })
      .collect( toList());
    }

  /**
   * Returns base names for all test resources with the given prefix.
   */
  private static Stream<String> getRequestCaseBaseNames( String prefix)
    {
    return
      getRequestCaseResources( prefix)
      .map( RealizeTest::getRequestCaseBaseName)
      .sorted();
    }

  /**
   * Returns all test resources with the given base name.
   */
  private static Stream<File> getRequestCaseResources( String baseName)
    {
    String testBaseName = trimToNull( System.getProperty( "testBaseName"));

    return
      testBaseName == null?
      Arrays.stream( getRequestCasesDir().listFiles( baseNameRequestCases( baseName))) :

      testBaseName.startsWith( baseName)?
      Arrays.stream( getRequestCasesDir().listFiles( baseNameRequestCases( testBaseName))) :

      Stream.empty();      
    }

  /**
   * Returns the base name of the given request test definition file.
   */
  private static String getRequestCaseBaseName( File testDefFile)
    {
    return testDefFile.getName().replaceAll( requestCasesSuffix_, "");
    }

  /**
   * Returns a filter that matches all test resources with the given base name.
   */
  private static FilenameFilter baseNameRequestCases( String baseName)
    {
    return
      FileFilterUtils.and(
        FileFilterUtils.prefixFileFilter( baseName),
        FileFilterUtils.suffixFileFilter( requestCasesSuffix_));
    }

  /**
   * Returns the location of the resource files for this class.
   */
  private static File getRequestCasesDir()
    {
    try
      {
      return new File( RealizeTest.class.getResource( ".").toURI().getPath());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get resource directory path", e);
      }
    }
  
  /**
   * Creates a new RealizeTest instance.
   */
  public RealizeTest( String testDefBase)
    {
    testDefBase_ = testDefBase;
    }

  @Test
  public void verifyRealized()
    {
    // Given...
    RequestTestDef requestTestDef = readRequestCases( requestCasesFor( testDefBase_));

    // When...
    RequestTestDef realized = RequestCases.realizeRequestCases( requestTestDef);
    
    // Then...
    verifyRealized( realized);
    }

  /**
   * Verifies that request cases match expectations.
   */
  private void verifyRealized( RequestTestDef requestTestDef)
    {
    if( acceptAsExpected())
      {
      updateRealizedCases( testDefBase_, requestTestDef);
      }
    else
      {
      assertThat(
        testDefBase_,
        requestTestDef.getRequestCases(),
        listsMembers( RequestCaseMatcher::new, readRequestCases( realizedCasesFor( testDefBase_)).getRequestCases()));
      }
    }

  /**
   * Returns the {@link RequestCase} objects represented by the given document resource.
   */
  protected RequestTestDef readRequestCases( String resource)
    {
    InputStream document = getClass().getResourceAsStream( resource);
    assertThat( "Request cases " + resource, document, is( notNullValue()));
    
    try( RequestTestDefReader reader = new RequestTestDefReader( document))
      {
      return reader.getRequestTestDef();
      }
    }

  /**
   * Updates the given {@link RequestCase} resource.
   */
  private void updateRealizedCases( String baseName, RequestTestDef requestTestDef)
    {
    File requestCaseFile = new File( saveExpectedDir_, realizedCasesFor( baseName));
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
    return String.format( "%s%s", baseName, requestCasesSuffix_);
    }

  /**
   * Returns the name of the realized request cases file with the given base name
   */
  private String realizedCasesFor( String baseName)
    {
    return String.format( "%s%s", baseName, realizedCasesSuffix_);
    }

  /**
   * Returns true if all generated results are automatically accepted.
   */
  private boolean acceptAsExpected()
    {
    return saveExpectedDir_ != null;
    }

  private String testDefBase_;

  private static final String requestCasesSuffix_ = "-Request-Cases.json";
  private static final String realizedCasesSuffix_ = "-Realized-Cases.json";

  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);
  }
