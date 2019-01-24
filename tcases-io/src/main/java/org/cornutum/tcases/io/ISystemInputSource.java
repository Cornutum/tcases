//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;

/**
 * Interface for reading a {@link SystemInputDef} instance.
 *
 */
public interface ISystemInputSource
  {
  /**
   * Returns a {@link SystemInputDef} instance.
   */
  SystemInputDef getSystemInputDef();
  }
