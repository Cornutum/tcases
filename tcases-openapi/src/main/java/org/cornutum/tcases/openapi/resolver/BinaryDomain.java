//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Arrays;
import java.util.Random;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

/**
 * Define a set of byte arrays that can be used by a request.
 */
public class BinaryDomain extends BaseStringDomain<byte[]>
  {
  /**
   * Creates a new BinaryDomain instance.
   */
  protected BinaryDomain()
    {
    this( 4096);
    }
  
  /**
   * Creates a new BinaryDomain instance.
   */
  protected BinaryDomain( int maxLength)
    {
    super( maxLength);
    }

  /**
   * Changes the values excluded from this domain.
   */
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
  protected int getLength( byte[] value)
    {
    return value.length;
    }

  /**
   * Returns true if the given values are equal.
   */
  protected boolean valuesEqual( byte[] value1, byte[] value2)
    {
    return Arrays.equals( value1, value2);
    }

  /**
   * Returns a new byte array.
   */
  private byte[] newValue( Random random)
    {
    byte[] bytes = new byte[ getLengthRange().select( random)];
    random.nextBytes( bytes);
    return bytes;
    }

  /**
   * Returns a random sequence of values from this domain.	
   */
  public Stream<byte[]> values( Random random)
    {
    return
      Stream.generate( () -> newValue( random))
      .filter( value -> isNotExcluded( value, getExcluded()));
    }
  }
