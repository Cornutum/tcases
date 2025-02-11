//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.annotations.AnnotationFinder;

import java.io.File;
import java.util.Collection;
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
  public static Optional<TestWriter<?,?>> createTestWriter( String name, TestCaseWriter testCaseWriter, Collection<File> extensions)
    {
    return
      annotationFinder()
      .annotation( ApiTestWriter.class)
      .inClasses( extensions)
      .find()
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
  public static Optional<TestCaseWriter> createTestCaseWriter( String name, Collection<File> extensions)
    {
    return
      annotationFinder()
      .annotation( ApiTestCaseWriter.class)
      .inClasses( extensions)
      .find()
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
  public static Optional<TestTarget> createTestTarget( Class<? extends TestWriter<?,?>> testWriterClass, Collection<File> extensions)
    {
    return
      Optional.of( testWriterClass)
      .flatMap( c -> Optional.ofNullable( c.getAnnotation( ApiTestWriter.class)))
      .flatMap( annotation -> Optional.ofNullable( annotation.target()))
      .flatMap( targetName -> createTestTarget( targetName, extensions));      
    }

  /**
   * Returns an instance of the {@link TestTarget} implementation annotated with the given name.
   */
  public static Optional<TestTarget> createTestTarget( String name, Collection<File> extensions)
    {
    return
      annotationFinder()
      .annotation( ApiTestTarget.class)
      .inClasses( extensions)
      .find()
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

  /**
   * Returns an {@link AnnotationFinder} to identify {@link TestWriter} implementations.
   */
  private static AnnotationFinder annotationFinder()
    {
    return
      new AnnotationFinder()
      .inPackage( "org.cornutum.tcases.openapi.testwriter")
      .inPackage( "org.cornutum.tcases.openapi.restassured");
    }

  }
