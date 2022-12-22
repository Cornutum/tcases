//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.Collection;
import java.util.Optional;

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
  @Override
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
   * Changes the format for values that belong to this domain.
   */
  @Override
  public void setFormat( String format)
    {
    super.setFormat(
      "int64".equals( format)
      ? getFormat()
      : format);
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  @Override
  public String getFormat()
    {
    return Optional.ofNullable( super.getFormat()).orElse( "int32");
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<Integer> dataValueOf( Integer value)
    {
    return new IntegerValue( value);
    }
  }
