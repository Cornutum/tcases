//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.IGeneratorSet;

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
