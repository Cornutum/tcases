//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotations;

import eu.infomas.annotation.AnnotationDetector;
import eu.infomas.annotation.AnnotationDetector.TypeReporter;

import java.io.File;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toCollection;

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
    annotation_ = annotation;
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
    return inClasses( packageClasses( packageNames));
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
    for( File file : classPath)
      {
      searchable_.add( file);
      }
    
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

        loadable_.addAll( searchable_);
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
        getClasses()
        .map( className -> forName( className));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't find classes with annotation=%s", annotation_.getSimpleName()), e);
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

  /**
   * Returns the class path elements that contain the given packages
   */
  private Set<File> packageClasses( Collection<String> packageNames)
    {
    return
      packageNames
      .stream()

      .flatMap( packageName -> {
        try
          {
          return
            Collections.list(
              Thread.currentThread().getContextClassLoader().getResources(
                Arrays.asList( packageName.split( "\\."))
                .stream()
                .collect( joining( "/", "", "/"))))
            .stream();
          }
        catch( Exception e)
          {
          throw new IllegalArgumentException( String.format( "Can't get resources for package=%s", packageName), e);
          }
        })

      .map( url -> {
        Optional<File> classPathFile;
        classPathFile =
          "file".equals( url.getProtocol())?
          toClassFile( url) :
          
          "jar".equals( url.getProtocol())?
          toJarFile( url) :

          Optional.empty();

        return classPathFile;
        })

      .filter( Optional::isPresent)
      .map( Optional::get)

      .collect( toCollection( LinkedHashSet::new));
    }

  /**
   * If the given URL represents a class file or directory, returns the file path.
   */
  private Optional<File> toClassFile( URL url)
    {
    try
      {
      return
        Optional.of( toFile( url))
        .filter( file -> file.isDirectory() || file.getPath().endsWith( ".class"));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't get file from url=%s", url), e);
      }
    }

  /**
   * If the given URL represents a JAR file, returns the file path.
   */
  private Optional<File> toJarFile( URL url)
    {
    try
      {
      JarURLConnection jar = (JarURLConnection)url.openConnection();
      return
        Optional.of( jar.getJarFileURL())
        .filter( jarUrl -> "file".equals( jarUrl.getProtocol()))
        .map( jarUrl -> toFile( jarUrl));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't get JAR file from url=%s", url), e);
      }
    }

  /**
   * Returns the file represented by the given URL:
   */
  private File toFile( URL url)
    {
    try
      {
      return new File( url.toURI().getPath());
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't get file from url=%s", url), e);
      }
    }

  /**
   * Returns a stream of annotated classes.
   */
  private Stream<String> getClasses() throws IOException
    {
    ClassAnnotationReporter reporter = new ClassAnnotationReporter();
    new AnnotationDetector( reporter).detect( loadable_.stream().toArray( File[]::new));
    return reporter.getStream();
    }

  /**
   * Creates a stream of annotated classes.
   */
  private class ClassAnnotationReporter implements TypeReporter
    {
    /**
     * Creates a new ClassAnnotationReporter instance.
     */
    @SuppressWarnings("unchecked")
	public ClassAnnotationReporter()
      {
      annotations_ = new Class[] {annotation_};
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

    private Class<? extends Annotation>[] annotations_;
    private Stream.Builder<String> builder_;
    }

  private Class<? extends Annotation> annotation_;
  private Set<File> searchable_ = new LinkedHashSet<File>();
  private Set<File> loadable_ = new LinkedHashSet<File>();
  }
