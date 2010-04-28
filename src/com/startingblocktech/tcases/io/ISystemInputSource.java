//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.SystemInputDef;

/**
 * Interface for reading a {@link SystemInputDef} instance.
 *
 * @version $Revision$, $Date$
 */
public interface ISystemInputSource
  {
  /**
   * Returns a {@link SystemInputDef} instance.
   */
  SystemInputDef getSystemInputDef();
  }
