//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Base class for annotated elements.
 *
 */
public abstract class Annotated
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
  public String getAnnotation( String name)
    {
    return annotations_.get( name);
    }

  /**
   * Returns the list of all annotation names.
   */
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
  
  private Map<String,String> annotations_ = new HashMap<String,String>();
  }

