//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.util.CollectionUtils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import static java.util.stream.Collectors.toList;

/**
 * Base class for annotated elements.
 *
 */
public abstract class Annotated implements IAnnotated
  {
  /**
   * Changes the value of the given annotation.
   */
  public void setAnnotation( String name, String value)
    {
    String annotation = StringUtils.trimToNull( name);
    if( annotation == null)
      {
      throw new IllegalArgumentException( "Annotation name must be non-blank");
      }
    if( value == null)
      {
      annotations_.remove( annotation);
      }
    else
      {
      annotations_.put( annotation, value);
      }
    }
  
  /**
   * Changes the value of the given annotation to a CSV string representing the given list.
   * @see CollectionUtils#toCsv
   */
  public void setAnnotationList( String name, Iterable<?> values)
    {
    setAnnotation(
      name,
      Optional.ofNullable( values)
      .map( list -> CollectionUtils.toCsv( CollectionUtils.toStream( list)))
      .orElse( null));
    }

  /**
   * Adds annotations from another annotated element. This leaves all existing annotation
   * unchanged, adding only annotations not already defined.
   */
  public void addAnnotations( Annotated other)
    {
    for( Iterator<String> otherAnnotations = other.getAnnotations(); otherAnnotations.hasNext();)
      {
      String name = otherAnnotations.next();
      if( getAnnotation( name) == null)
        {
        setAnnotation( name, other.getAnnotation( name));
        }
      }
    }

  /**
   * Returns the value of the given annotation.
   */
  @Override
  public String getAnnotation( String name)
    {
    return annotations_.get( name);
    }

  /**
   * Returns the {@link #setAnnotationList list represented by the value} of the given annotation.
   * @see #setAnnotationList
   */
  public List<String> getAnnotationList( String name)
    {
    return
      Optional.ofNullable( getAnnotation( name))
      .map( value -> CollectionUtils.fromCsv( value).collect( toList()))
      .orElse( null);
    }

  /**
   * Returns the list of all annotation names.
   */
  @Override
  public Iterator<String> getAnnotations()
    {
    return annotations_.keySet().iterator();
    }

  /**
   * Returns the number of annotations defined.
   */
  public int getAnnotationCount()
    {
    return annotations_.size();
    }

  /**
   * The standard annotation for test case properties.
   */
  public static final String TEST_CASE_PROPERTIES = "properties";
  
  private Map<String,String> annotations_ = new HashMap<String,String>();
  }

