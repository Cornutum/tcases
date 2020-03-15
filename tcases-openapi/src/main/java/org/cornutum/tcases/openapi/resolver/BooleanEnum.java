//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Collection;

/**
 * Defines an enumerated boolean value set.
 */
public class BooleanEnum extends EnumDomain<Boolean>
  {
  /**
   * Creates a new BooleanEnum instance.
   */
  public BooleanEnum( Iterable<String> enums)
    {
    super( Type.BOOLEAN, enums);
    }
  
  /**
   * Creates a new BooleanEnum instance.
   */
  public BooleanEnum( Collection<Boolean> enums)
    {
    super( Type.BOOLEAN, enums);
    }

  /**
   * Returns the value represented by the given string.
   */
  protected Boolean valueOf( String value)
    {
    return Boolean.valueOf( value);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Boolean> dataValueOf( Boolean value)
    {
    return new BooleanValue( value);
    }
  }
