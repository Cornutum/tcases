//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Defines a singleton Boolean value set.
 */
public class BooleanConstant extends ConstantDomain<Boolean>
  {
  /**
   * Creates a new BooleanConstant instance.
   */
  public BooleanConstant( Boolean value)
    {
    super( Type.BOOLEAN, value);
    }
  }
