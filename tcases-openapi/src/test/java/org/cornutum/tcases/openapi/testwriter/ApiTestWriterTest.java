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

/**
 * Runs tests for the {@link ApiTestWriter} annotation.
 */
public class ApiTestWriterTest
  {
  @Test
  public void junitCreated()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "junit", null);
    
    // Then...
    assertThat( "JUnitTestWriter found", created.isPresent(), is( true));

    TestWriter<?,?> testWriter = created.get();
    assertThat( "JUnitTestWriter", testWriter.getClass(), is( JUnitTestWriter.class));
    }
  
  @Test
  public void testngCreated()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "testng", null);
    
    // Then...
    assertThat( "TestNgTestWriter found", created.isPresent(), is( true));

    TestWriter<?,?> testWriter = created.get();
    assertThat( "TestNgTestWriter", testWriter.getClass(), is( TestNgTestWriter.class));
    }
  
  @Test
  public void notFound()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "?", null);
    
    // Then...
    assertThat( "TestWriter found", created.isPresent(), is( false));
    }

  @Test
  public void notTestWriter()
    {
    // When...
    Optional<TestWriter<?,?>> created = createTestWriter( "notTestWriter", null);
    
    // Then...
    assertThat( "TestWriter found", created.isPresent(), is( false));
    }

  @Test
  public void targetCreated()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( JUnitTestWriter.class);
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( true));

    TestTarget target = created.get();
    assertThat( "JavaTestTarget", target.getClass(), is( JavaTestTarget.class));
    }

  @Test
  public void defaultTargetCreated()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( DefaultTarget.class);
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( true));

    TestTarget target = created.get();
    assertThat( "JavaTestTarget", target.getClass(), is( JavaTestTarget.class));
    }

  @Test
  public void notTarget()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( NonTarget.class);
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( false));
    }

  @Test
  public void notAnnotated()
    {
    // When...
    Optional<TestTarget> created = createTestTarget( NotAnnotated.class);
    
    // Then...
    assertThat( "TestTarget found", created.isPresent(), is( false));
    }

  @Test
  public void undefinedTarget()
    {
    expectFailure( IllegalStateException.class)
      .when( () -> createTestTarget( UndefinedTarget.class))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Can't load class=org.cornutum.tcases.UndefinedTarget"));
        });
    }

  @Test
  public void undefinedTargetConstructor()
    {
    expectFailure( IllegalStateException.class)
      .when( () -> createTestTarget( UndefinedTargetConstructor.class))
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

  @ApiTestWriter( name="nonTarget", targetClass="org.cornutum.tcases.openapi.testwriter.ApiTestWriterTest$NotTarget")
  private static class NonTarget extends JUnitTestWriter
    {
    public NonTarget( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @SuppressWarnings("unused")
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

  @ApiTestWriter( name="UndefinedTarget", targetClass="org.cornutum.tcases.UndefinedTarget")
  private static class UndefinedTarget extends JUnitTestWriter
    {
    public UndefinedTarget( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @ApiTestWriter( name="UndefinedTargetConstructor", targetClass="org.cornutum.tcases.openapi.testwriter.ApiTestWriterTest$NoDefaultConstructor")
  private static class UndefinedTargetConstructor extends JUnitTestWriter
    {
    public UndefinedTargetConstructor( TestCaseWriter testCaseWriter)
      {
      super( testCaseWriter);
      }
    }

  @SuppressWarnings("unused")
  private static class NoDefaultConstructor extends TestTarget
    {
    public NoDefaultConstructor( String arg)
      {
      }
    }

  }
