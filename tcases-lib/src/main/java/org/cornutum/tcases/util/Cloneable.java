//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

/**
 * Defines an interface for creating an object copy.
 */
public interface Cloneable<T>
  {
  /**
   * Returns a copy of this object.
   */
  T cloneOf();
  }
