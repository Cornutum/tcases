//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Arrays;

/**
 * Defines a singleton byte array value set.
 */
public class BinaryConstant extends ConstantDomain<byte[]>
  {
  /**
   * Creates a new BinaryConstant instance.
   */
  public BinaryConstant( byte[] value)
    {
    super( Type.STRING, value);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( byte[] value)
    {
    return Arrays.equals( value, getValue());
    }
  }
