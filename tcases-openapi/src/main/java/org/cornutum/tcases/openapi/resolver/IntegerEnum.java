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
 * Defines an enumerated integer value set.
 */
public class IntegerEnum extends EnumDomain<Integer>
  {
  /**
   * Creates a new IntegerEnum instance.
   */
  public IntegerEnum( Iterable<String> enums)
    {
    super( Type.INTEGER, enums);
    }
  
  /**
   * Creates a new IntegerEnum instance.
   */
  public IntegerEnum( Collection<Integer> enums)
    {
    super( Type.INTEGER, enums);
    }

  /**
   * Returns the value represented by the given string.
   */
  protected Integer valueOf( String value)
    {
    try
      {
      return Integer.valueOf( value);
      }
    catch( Exception e)
      {
      throw new ValueDomainException( String.format( "Value=%s is not a valid integer number", value), e);
      }
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Integer> dataValueOf( Integer value)
    {
    return new IntegerValue( value);
    }
  }
