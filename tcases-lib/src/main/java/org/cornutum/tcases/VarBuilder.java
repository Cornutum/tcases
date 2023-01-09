//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2023, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

/**
 * Base class for building {@link IVarDef} instances.
 */
public abstract class VarBuilder<T extends VarBuilder<T>> extends AnnotatedBuilder<T>
  {
  /**
   * Returns the {@link IVarDef} instance for this builder.
   */
  protected abstract IVarDef getVarDef();
  }
