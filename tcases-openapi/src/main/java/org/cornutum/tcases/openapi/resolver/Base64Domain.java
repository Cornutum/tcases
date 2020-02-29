//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;

import java.util.Base64;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;
import static java.util.Collections.emptySet;

/**
 * Define a set of base64-encoded byte arrays that can be used by a request.
 */
public class Base64Domain extends SequenceDomain<String>
  {
  /**
   * Creates a new Base64Domain instance.
   */
  public Base64Domain()
    {
    this( 8192);
    }
  
  /**
   * Creates a new Base64Domain instance.
   */
  public Base64Domain( int maxLength)
    {
    super( maxLength);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new Base64Value( value);
    }

  /**
   * Changes the values excluded from this domain.
   */
  public void setExcludedStrings( Set<String> excluded)
    {
    setExcluded( excluded);
    }

  /**
   * Returns the length of the given value.
   */
  protected int getLength( String value)
    {
    return decoded( value).length;
    }

  /**
   * Defines a constant length range for values in this domain.
   */
  public void setLengthRange( Integer length)
    {
    getBytes().setLengthRange( length);
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Integer min, Integer max)
    {
    getBytes().setLengthRange( min, max);
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Range range)
    {
    getBytes().setLengthRange( range);
    }

  /**
   * Returns the length range for values in this domain.
   */
  protected ValueDomain<Integer> getLengthRange()
    {
    return getBytes().getLengthRange();
    }

  /**
   * Changes the values excluded from this domain.
   */
  public void setExcluded( Set<String> excluded)
    {
    getBytes().setExcluded(
      Optional.ofNullable( excluded).orElse( emptySet())
      .stream()
      .map( Base64Domain::decoded)
      .collect( toSet()));
    }

  /**
   * Returns the values excluded from this domain.
   */
  public Set<String> getExcluded()
    {
    return
      getBytes().getExcluded().stream()
      .map( Base64Domain::encoded)
      .collect( toSet());
    }

  /**
   * Returns the BinaryDomain delegate for this domain.
   */
  private BinaryDomain getBytes()
    {
    if( bytes_ == null)
      {
      bytes_ = new BinaryDomain( getMaxLength());
      }

    return bytes_;
    }

  /**
   * Returns the base64 encoding of the given bytes.
   */
  protected static String encoded( byte[] bytes)
    {
    return Base64.getEncoder().encodeToString( bytes);
    }

  /**
   * Returns the bytes represented by the given base64 encoding.
   */
  protected static byte[] decoded( String base64)
    {
    return Base64.getDecoder().decode( base64);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    try
      {
      return getBytes().contains( decoded( value));
      }
    catch( Exception e)
      {
      return false;
      }
    }
  
  /**
   * Returns a random sequence of possible members of this domain.
   */
  protected Stream<String> candidates( ResolverContext context)
    {
    return
      getBytes().values( context)
      .map( DataValue::getValue)
      .map( Base64Domain::encoded);
    }

  private BinaryDomain bytes_;
  }
