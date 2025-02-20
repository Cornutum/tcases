//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotations;

import org.cornutum.annotation.Annotated;
import org.cornutum.annotation.Finder;
import org.cornutum.annotation.PackageFilter;
import static org.cornutum.annotation.Files.classPathFor;

import java.io.File;
import java.lang.annotation.Annotation;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Finds annotated classes at runtime.
 */
public class AnnotationFinder
  {
  /**
   * Creates a new AnnotationFinder instance.
   */
  public AnnotationFinder()
    {
    finder_ = new Finder().filter( filter_);
    }
  
  /**
   * Creates a new AnnotationFinder instance.
   */
  public AnnotationFinder( Class<? extends Annotation> annotation)
    {
    this();
    annotation( annotation);
    }

  /**
   * Find classes with the given annotation.
   */
  public AnnotationFinder annotation( Class<? extends Annotation> annotation)
    {
    filter_.annotation( annotation);
    return this;
    }

  /**
   * Find annotated classes that belong to one of the given packages
   */
  public AnnotationFinder inPackage( String... packageNames)
    {
    return inPackage( Arrays.asList( packageNames));
    }

  /**
   * Find annotated classes that belong to one of the given packages
   */
  public AnnotationFinder inPackage( Collection<String> packageNames)
    {
    return inClasses( classPathFor( packageNames));
    }

  /**
   * Find annotated classes among one of the given class path elements.
   * Each file must be a *.class file, a directory, or a JAR file.
   */
  public AnnotationFinder inClasses( File... classPath)
    {
    return inClasses( Arrays.asList( classPath));
    }

  /**
   * Find annotated classes among one of the given class path elements.
   * Each file must be a *.class file, a directory, or a JAR file.
   */
  public AnnotationFinder inClasses( Collection<File> classPath)
    {
    finder_.inClasses( classPath);
    searchable_.addAll( classPath);
    
    return this;
    }

  /**
   * Returns annotated classes found in the specified packages or class path elements.
   */
  public Stream<Class<?>> find()
    {
    try
      {
      if( !searchable_.isEmpty())
        {
        Thread.currentThread().setContextClassLoader
          ( new URLClassLoader(
              searchable_.stream()
              .map( file -> {
                try
                  {
                  return file.toURI().toURL();
                  }
                catch( Exception e)
                  {
                  throw new IllegalArgumentException( String.format( "Can't get URL for file=%s", file), e);
                  }
                })
              .toArray( URL[]::new),
              Thread.currentThread().getContextClassLoader()));

        searchable_.clear();
        }
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't update searchable class path", e);
      }
    try
      {
      return
        finder_.find()
        .filter( Annotated::isClass)
        .map( annotated -> forName( annotated.getClassName()));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't find classes with annotation=%s", filter_.getAnnotations()), e);
      }
    }

  /**
   * Loads the given class.
   */
  public static Class<?> forName( String className) throws IllegalStateException
    {
    try
      {
      return Thread.currentThread().getContextClassLoader().loadClass( className);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't load class=%s", className), e);
      }
    }

  private Finder finder_;
  private PackageFilter filter_ = new PackageFilter();
  private Set<File> searchable_ = new LinkedHashSet<File>();
  }
