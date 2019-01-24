//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemTestDef;

/**
 * Interface for reading a {@link SystemTestDef} instance.
 *
 */
public interface ISystemTestSource
  {
  /**
   * Returns a {@link SystemTestDef} instance.
   */
  SystemTestDef getSystemTestDef();
  }
