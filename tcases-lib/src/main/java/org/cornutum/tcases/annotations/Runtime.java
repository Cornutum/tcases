//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotations;

import eu.infomas.annotation.AnnotationDetector;
import eu.infomas.annotation.AnnotationDetector.TypeReporter;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.util.stream.Stream;

/**
 * Finds annotated classes at runtime.
 */
public final class Runtime
  {
  private Runtime()
    {
    // Static methods only
    }

  /**
   * Returns all classes with the given annotation.
   */
  public static Stream<Class<?>> annotatedClasses( Class<? extends Annotation> annotation)
    {
    try
      {
      return
        ClassAnnotationReporter.getClasses( annotation)
        .map( className -> forName( className));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't find classes with annotation=%s", annotation.getSimpleName()), e);
      }
    }

  /**
   * Loads the given class.
   */
  public static Class<?> forName( String className) throws IllegalStateException
    {
    try
      {
      return Class.forName( className);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't load class=%s", className), e);
      }
    }

  /**
   * Creates a stream of annotated classes.
   */
  private static class ClassAnnotationReporter implements TypeReporter
    {
    /**
     * Creates a new ClassAnnotationReporter instance.
     */
    @SuppressWarnings("unchecked")
	public ClassAnnotationReporter( Class<? extends Annotation> annotation)
      {
      annotations_ = new Class[] {annotation};
      builder_ = Stream.builder();
      }

    @Override
	public void reportTypeAnnotation( Class<? extends Annotation> annotation, String className)
      {
      builder_.add( className);
      }

    @Override
	public Class<? extends Annotation>[] annotations()
      {
      return annotations_;
      }

    /**
     * Returns a stream of reported classes.
     */
    public Stream<String> getStream()
      {
      return builder_.build();
      }

    /**
     * Returns a stream of annotated classes.
     */
    public static Stream<String> getClasses( Class<? extends Annotation> annotation) throws IOException
      {
      ClassAnnotationReporter reporter = new ClassAnnotationReporter( annotation);
      new AnnotationDetector( reporter).detect();
      return reporter.getStream();
      }

    private Class<? extends Annotation>[] annotations_;
    private Stream.Builder<String> builder_;
    }
  }
