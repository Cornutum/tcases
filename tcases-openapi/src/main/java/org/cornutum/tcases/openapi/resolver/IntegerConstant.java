//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Defines a singleton Integer value set.
 */
public class IntegerConstant extends ConstantDomain<Integer>
  {
  /**
   * Creates a new IntegerConstant instance.
   */
  public IntegerConstant( Integer value)
    {
    super( Type.INTEGER, value);
    }
  }
