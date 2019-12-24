//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Defines a singleton String value set.
 */
public class StringConstant extends ConstantDomain<String>
  {
  /**
   * Creates a new StringConstant instance.
   */
  public StringConstant( String value)
    {
    super( Type.STRING, value);
    }
  }
