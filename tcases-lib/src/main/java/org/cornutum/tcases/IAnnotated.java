//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Iterator;

/**
 * An annotated element.
 *
 */
public interface IAnnotated
  {
  /**
   * Returns the value of the given annotation.
   */
  String getAnnotation( String name);

  /**
   * Returns the list of all annotation names.
   */
  Iterator<String> getAnnotations();
  }

