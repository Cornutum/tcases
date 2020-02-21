//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.FunctionTestDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.io.SystemTestResource;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.junit.Test;
import static org.apache.commons.lang3.StringUtils.trimToNull;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link RequestCaseDefiner}.
 */
public class RequestCaseDefinerTest
  {
  @Test
  public void forOperations()
    {
    getRequestCaseDefs( "operations");
    }
  
  @Test
  public void forIntegers()
    {
    getRequestCaseDefs( "integer");
    }
  
  @Test
  public void forNumbers()
    {
    getRequestCaseDefs( "number");
    }
  
  @Test
  public void forStrings()
    {
    getRequestCaseDefs( "string");
    }
  
  @Test
  public void forObjects()
    {
    getRequestCaseDefs( "object");
    }
  
  @Test
  public void forArrays()
    {
    getRequestCaseDefs( "array");
    }
  
  @Test
  public void forAllOfs()
    {
    getRequestCaseDefs( "allOf");
    }
  
  @Test
  public void forAnyOfs()
    {
    getRequestCaseDefs( "anyOf");
    }
  
  @Test
  public void forOneOfs()
    {
    getRequestCaseDefs( "oneOf");
    }
  
  @Test
  public void forNots()
    {
    getRequestCaseDefs( "not");
    }

  /**
   * Generate request cases for every test resource with the given base name.
   */
  private void getRequestCaseDefs( String baseName)
    {
    // Given...
    RequestCaseDefiner definer = new RequestCaseDefiner();
    Stream<File> testResources = getRequestTestResources( baseName);

    testResources.forEach(
      file -> {
      // When...
      try
        {
        SystemTestDef testDef = SystemTestResource.of( file).getSystemTestDef();
        List<RequestCaseDef> requestCaseDefs = getRequestCaseDefs( definer, testDef);

        // Then...
        for( RequestCaseDef requestCaseDef : requestCaseDefs)
          {
          System.out.println( String.format( "%s: %s", file, requestCaseDef));
          }
        }
      catch( Exception e)
        {
        throw new RequestCaseException( String.format( "Can't get request case from file=%s", file.getName()), e);
        }
      });
    }

  /**
   * Returns the location of the resource file for the given class.
   */
  private File getResourceDir()
    {
    try
      {
      return new File( getClass().getResource( "..").toURI().getPath());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get resource directory path", e);
      }
    }

  /**
   * Returns all request test resources with the given base name.
   */
  private Stream<File> getRequestTestResources( String baseName)
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
  private FilenameFilter baseNameResources( String baseName)
    {
    return
      FileFilterUtils.and(
        FileFilterUtils.prefixFileFilter( baseName),
        FileFilterUtils.suffixFileFilter( "-Expected-Test.xml"));
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
      .map( testCase -> {
        try
          {
          return definer.toRequestCaseDef( testCase);
          }
        catch( Exception e)
          {
          throw new RequestCaseException( String.format( "Can't get request case for test case=%s", testCase.getId()), e);
          }
        })
      .collect( toList());
    }

  }
