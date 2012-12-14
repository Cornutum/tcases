//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator.io;

import com.startingblocktech.tcases.generator.IGeneratorSet;

/**
 * Interface for reading an {@link IGeneratorSet} instance.
 *
 * @version $Revision$, $Date$
 */
public interface IGeneratorSetSource
  {
  /**
   * Returns a {@link IGeneratorSet} instance.
   */
  IGeneratorSet getGeneratorSet();
  }
