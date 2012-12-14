//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.SystemTestDef;

/**
 * Interface for reading a {@link SystemTestDef} instance.
 *
 * @version $Revision$, $Date$
 */
public interface ISystemTestSource
  {
  /**
   * Returns a {@link SystemTestDef} instance.
   */
  SystemTestDef getSystemTestDef();
  }
