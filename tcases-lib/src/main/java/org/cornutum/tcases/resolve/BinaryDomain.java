//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Stream;


import static java.util.stream.Collectors.toSet;

/**
 * Define a set of byte arrays that can be used by a request.
 */
public class BinaryDomain extends SequenceDomain<byte[]>
  {
  /**
   * Creates a new BinaryDomain instance.
   */
  public BinaryDomain()
    {
    this( 8192);
    }
  
  /**
   * Creates a new BinaryDomain instance.
   */
  public BinaryDomain( int maxLength)
    {
    super( maxLength);
    }

  /**
   * Changes the format for values that belong to this domain.
   */
  @Override
  public void setFormat( String format)
    {
    // This domain is defined by a specific format
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  @Override
  public String getFormat()
    {
    return "binary";
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<byte[]> dataValueOf( byte[] value)
    {
    return new BinaryValue( value);
    }

  /**
   * Changes the values excluded from this domain.
   */
  @Override
  public void setExcludedStrings( Set<String> excluded)
    {
    setExcluded(
      excluded.stream()
      .map( Base64Domain::decoded)
      .collect( toSet()));
    }

  /**
   * Returns the length of the given value.
   */
  @Override
  protected int getLength( byte[] value)
    {
    return value.length;
    }

  /**
   * Returns true if the given values are equal.
   */
  @Override
  protected boolean valuesEqual( byte[] value1, byte[] value2)
    {
    return Arrays.equals( value1, value2);
    }

  /**
   * Returns a new byte array.
   */
  private byte[] newValue( ResolverContext context)
    {
    byte[] bytes = new byte[ getLengthRange().selectValue( context)];
    context.getRandom().nextBytes( bytes);
    return bytes;
    }
  
  /**
   * Returns a random sequence of possible members of this domain.
   */
  @Override
  protected Stream<byte[]> candidates( ResolverContext context)
    {
    return Stream.generate( () -> newValue( context));
    }
  }
