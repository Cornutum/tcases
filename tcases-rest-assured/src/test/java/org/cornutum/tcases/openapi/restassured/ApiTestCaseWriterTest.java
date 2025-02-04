//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.cornutum.tcases.openapi.testwriter.ApiTestCaseWriter;
import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;

import static org.cornutum.tcases.openapi.testwriter.Runtime.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Optional;

/**
 * Runs tests for the {@link ApiTestCaseWriter} annotation.
 */
public class ApiTestCaseWriterTest
  {
  @Test
  public void created()
    {
    // When...
    Optional<TestCaseWriter> created = createTestCaseWriter( "restassured");
    
    // Then...
    assertThat( "TestCaseWriter found", created.isPresent(), is( true));

    TestCaseWriter testCaseWriter = created.get();
    assertThat( "RestAssuredTestCaseWriter", testCaseWriter.getClass(), is( RestAssuredTestCaseWriter.class));
    }

  @Test
  public void notFound()
    {
    // When...
    Optional<TestCaseWriter> created = createTestCaseWriter( "?");
    
    // Then...
    assertThat( "TestCaseWriter found", created.isPresent(), is( false));
    }

  @Test
  public void notTestCaseWriter()
    {
    // When...
    Optional<TestCaseWriter> created = createTestCaseWriter( "notTestCaseWriter");
    
    // Then...
    assertThat( "TestCaseWriter found", created.isPresent(), is( false));
    }

  @ApiTestCaseWriter( name="notTestCaseWriter")
  private static class NotTestCaseWriter
    {
    }

  }
