//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.util.HashMap;
import java.util.regex.Pattern;

/**
 * Runs tests for the {@link TransformFilter}.
 *
 */
public class TestTransformFilter
  {
  @Test
  public void testWriteFile() throws Exception
    {
    // Given...
    TransformFilter filter = new TestDefToJUnitFilter(); 
    File target = File.createTempFile( "TestTransformFilter-WriteFile-", ".java");
    target.deleteOnExit();
    filter.setTarget( target);

    // When...
    IOUtils.copy
      ( getClass().getResourceAsStream( "system-test-def-2.xml"),
        filter.getSource());
    filter.getSource().close();
    
    // Then...
    assertThat( "Output written", target.length() > 0, is( true));

    String result = FileUtils.readFileToString( target, "UTF-8");
    assertThat
      ( "System tested=default",
        contains( result, "Tests \\{@link System-2#Function-0 Function-0\\(\\)\\}") ,
        is( true));
    assertThat
      ( "Throws=default",
        contains( result, "public void Function-0_0\\(\\)$"),
        is( true));
    }
  
  @Test
  public void testStandardOutput() throws Exception
    {
    // Given...
    TransformFilter filter = new TestDefToJUnitFilter();

    File target = null;
    filter.setTarget( target);

    // When...
    IOUtils.copy
      ( getClass().getResourceAsStream( "system-test-def-2.xml"),
        filter.getSource());
    filter.getSource().close();
    
    // Then...
    // Check standard output
    }
  
  @Test
  public void testWithParams() throws Exception
    {
    // Given...
    TransformFilter filter = new TestDefToJUnitFilter();

    File target = File.createTempFile( "TestTransformFilter-WithParams-", ".java");
    target.deleteOnExit();
    filter.setTarget( target);

    HashMap<String,Object> params = new HashMap<String,Object>();
    params.put( "throws", "true");
    params.put( "class", "SUT");
    filter.setParams( params);

    // When...
    IOUtils.copy
      ( getClass().getResourceAsStream( "system-test-def-2.xml"),
        filter.getSource());
    filter.getSource().close();
    
    // Then...
    assertThat( "Output written", target.length() > 0, is( true));

    String result = FileUtils.readFileToString( target, "UTF-8");
    assertThat
      ( "System tested=SUT",
        contains( result, "Tests \\{@link SUT#Function-0 Function-0\\(\\)\\}"),
        is( true));
    assertThat
      ( "Throws=Exception",
        contains( result, "public void Function-0_0\\(\\) throws Exception$"),
        is( true));
    }

  /**
   * Returns true if the given transform result contains a match for the given
   * regular expression.
   */
  private boolean contains( String result, String pattern)
    {
    return
      Pattern.compile( pattern, Pattern.MULTILINE)
      .matcher( result)
      .find();
    }
  }
