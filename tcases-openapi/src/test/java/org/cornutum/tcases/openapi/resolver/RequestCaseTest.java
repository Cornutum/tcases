//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.apache.commons.io.filefilter.FileFilterUtils;
import static org.apache.commons.lang3.StringUtils.trimToNull;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Base class for {@link RequestCase} tests
 */
public class RequestCaseTest
  {
  protected static Iterable<Object[]> getTestDefBaseNameParams()
    {
    return
      Arrays.stream( testGroups_)
      .flatMap( prefix -> getRequestTestBaseNames( prefix))
      .map( baseName -> new Object[]{ baseName })
      .collect( toList());
    }

  /**
   * Returns base names for all request test resources with the given prefix.
   */
  protected static Stream<String> getRequestTestBaseNames( String prefix)
    {
    return
      getRequestTestResources( prefix)
      .map( RequestCaseTest::getTestDefBaseName)
      .sorted();
    }

  /**
   * Returns all request test resources with the given base name.
   */
  protected static Stream<File> getRequestTestResources( String baseName)
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
        FileFilterUtils.suffixFileFilter( testDefSuffix_));
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
   * Returns the test definition resource file with the given base name.
   */
  protected File getTestDefFile( String baseName)
    {
    return new File( getResourceDir(), baseName + testDefSuffix_);
    }

  /**
   * Returns the base name of the given test definition file.
   */
  protected static String getTestDefBaseName( File testDefFile)
    {
    return testDefFile.getName().replaceAll( testDefSuffix_, "");
    }

  /**
   * Returns the Random seed for this test.
   */
  protected long getSeed()
    {
    return seed_;
    }
  
  /**
   * Returns the Random instance for this test.
   */
  protected Random getRandom()
    {
    return random_;
    }

  /**
   * Returns the {@link ResolverContext} for this test.
   */
  protected ResolverContext getResolverContext()
    {
    return new ResolverContext( getRandom());
    }

  private Random random_ = new Random( seed_);

  private static long seed_ =
    Optional.ofNullable( System.getProperty( "seed"))
    .map( Long::valueOf)
    .orElse( 6745393444958854970L);
  
  private static final String[] testGroups_ = new String[] {
    "allOf",
    "anyOf",
    "array",
    "emptySchema",
    "integer",
    "not",
    "number",
    "object",
    "oneOf",
    "operations",
    "petstore-requests",
    "string"
  };

  private static final String testDefSuffix_ = "-Expected-Test.xml";
  }
