//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import static org.cornutum.tcases.openapi.testwriter.Runtime.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Optional;
import static java.util.Collections.emptySet;

/**
 * Runs tests for the {@link ApiTestWriter} annotation.
 */
public class ApiTestWriterTest
  {
  @Test
  public void junitCreated()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "junit", null, emptySet());
    
    // Then...
    assertThat( "JUnitTestWriter found", created.isPresent(), is( true));

    TestWriter<?,?> testWriter = created.get();
    assertThat( "JUnitTestWriter", testWriter.getClass(), is( JUnitTestWriter.class));
    }
  
  @Test
  public void testngCreated()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "testng", null, emptySet());
    
    // Then...
    assertThat( "TestNgTestWriter found", created.isPresent(), is( true));

    TestWriter<?,?> testWriter = created.get();
    assertThat( "TestNgTestWriter", testWriter.getClass(), is( TestNgTestWriter.class));
    }
  
  @Test
  public void notFound()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "?", null, emptySet());
    
    // Then...
    assertThat( "TestWriter found", created.isPresent(), is( false));
    }

  @Test
  public void notTestWriter()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "notTestWriter", null, emptySet());
    
    // Then...
    assertThat( "TestWriter found", created.isPresent(), is( false));
    }

  @Test
  public void targetCreated()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( JUnitTestWriter.class, emptySet());
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( true));

    TestTarget target = created.get();
    assertThat( "JavaTestTarget", target.getClass(), is( JavaTestTarget.class));
    }

  @Test
  public void defaultTargetCreated()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( DefaultTarget.class, emptySet());
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( true));

    TestTarget target = created.get();
    assertThat( "JavaTestTarget", target.getClass(), is( JavaTestTarget.class));
    }

  @Test
  public void notTarget()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( NonTarget.class, emptySet());
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( false));
    }

  @Test
  public void notAnnotated()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( NotAnnotated.class, emptySet());
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( false));
    }

  @Test
  public void undefinedTarget()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( UndefinedTarget.class, emptySet());
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( false));
    }

  @Test
  public void undefinedTargetConstructor()
    {
    expectFailure( IllegalStateException.class)
      .when( () -> createTestTarget( UndefinedTargetConstructor.class, emptySet()))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Can't create instance of class=NoDefaultConstructor"));
        });
    }

  @ApiTestWriter( name="notTestWriter")
  private static class NotTestWriter
    {
    }

  @ApiTestWriter( name="defaultTarget")
  private static class DefaultTarget extends JUnitTestWriter
    {
    public DefaultTarget( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @ApiTestWriter( name="nonTarget", target="notTarget")
  private static class NonTarget extends JUnitTestWriter
    {
    public NonTarget( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @SuppressWarnings("unused")
  @ApiTestTarget( name="notTarget")
  private static class NotTarget
    {
    }

  private static class NotAnnotated extends JUnitTestWriter
    {
    public NotAnnotated( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @ApiTestWriter( name="UndefinedTarget", target="?")
  private static class UndefinedTarget extends JUnitTestWriter
    {
    public UndefinedTarget( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @ApiTestWriter( name="UndefinedTargetConstructor", target="noDefaultConstructor")
  private static class UndefinedTargetConstructor extends JUnitTestWriter
    {
    public UndefinedTargetConstructor( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @SuppressWarnings("unused")
  @ApiTestTarget( name="noDefaultConstructor")
  private static class NoDefaultConstructor extends TestTarget
    {
    public NoDefaultConstructor( String arg)
      {
      }
    }

  }
