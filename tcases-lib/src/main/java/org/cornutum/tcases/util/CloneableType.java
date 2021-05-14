//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

/**
 * Defines an interface for creating a copy of a specified type.
 */
public interface CloneableType<T>
  {
  /**
   * Returns a copy of this object.
   */
  T cloneOf();
  }
