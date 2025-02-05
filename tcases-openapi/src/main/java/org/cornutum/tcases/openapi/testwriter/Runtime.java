//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import static org.cornutum.tcases.annotations.Runtime.*;

import java.util.Optional;

/**
 * Creates {@link TestWriter} and {@link TestCaseWriter} instances specified for the current runtime environment.
 */
public final class Runtime
  {
  private Runtime()
    {
    // Static methods only
    }

  /**
   * Returns an instance of the {@link TestWriter} implementation annotated with the given name.
   */
  public static Optional<TestWriter<?,?>> createTestWriter( String name, TestCaseWriter testCaseWriter)
    {
    return
      annotatedClasses( ApiTestWriter.class)
      .filter( c -> name.equals( c.getAnnotation( ApiTestWriter.class).name()))
      .filter( c -> TestWriter.class.isAssignableFrom( c))
      .findFirst()
      .map( c -> {
        try
          {
          return (TestWriter<?,?>) c.getConstructor( TestCaseWriter.class).newInstance( testCaseWriter);
          }
        catch( Exception e)
          {
          throw new IllegalStateException( String.format( "Can't create instance of class=%s", c.getSimpleName()), e);
          }
        });
    }

  /**
   * Returns an instance of the {@link TestCaseWriter} implementation annotated with the given name.
   */
  public static Optional<TestCaseWriter> createTestCaseWriter( String name)
    {
    return
      annotatedClasses( ApiTestCaseWriter.class)
      .filter( c -> name.equals( c.getAnnotation( ApiTestCaseWriter.class).name()))
      .filter( c -> TestCaseWriter.class.isAssignableFrom( c))
      .findFirst()
      .map( c -> {
        try
          {
          return (TestCaseWriter) c.newInstance();
          }
        catch( Exception e)
          {
          throw new IllegalStateException( String.format( "Can't create instance of class=%s", c.getSimpleName()), e);
          }
        });
    }

  /**
   * Returns an instance of the {@link TestTarget} implementation annotated for the given {@link TestWriter} class.
   */
  public static Optional<TestTarget> createTestTarget( Class<? extends TestWriter<?,?>> testWriterClass)
    {
    return
      Optional.of( testWriterClass)
      .flatMap( c -> Optional.ofNullable( c.getAnnotation( ApiTestWriter.class)))
      .flatMap( annotation -> Optional.ofNullable( annotation.target()))
      .flatMap( targetName -> createTestTarget( targetName));      
    }

  /**
   * Returns an instance of the {@link TestTarget} implementation annotated with the given name.
   */
  public static Optional<TestTarget> createTestTarget( String name)
    {
    return
      annotatedClasses( ApiTestTarget.class)
      .filter( c -> name.equals( c.getAnnotation( ApiTestTarget.class).name()))
      .filter( c -> TestTarget.class.isAssignableFrom( c))
      .findFirst()
      .map( targetClass -> {
        try
          {
          return (TestTarget) targetClass.newInstance();
          }
        catch( Exception e)
          {
          throw new IllegalStateException( String.format( "Can't create instance of class=%s", targetClass.getSimpleName()), e);
          }
        });      
    }
  }
