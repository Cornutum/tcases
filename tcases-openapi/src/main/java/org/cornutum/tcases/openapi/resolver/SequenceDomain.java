//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.stream.Stream;

import static java.util.Collections.emptySet;

/**
 * Base class for domains that define a set of byte/character sequences that can be used by a request.
 */
public abstract class SequenceDomain<T> extends AbstractValueDomain<T>
  {
  /**
   * Creates a new SequenceDomain instance.
   */
  protected SequenceDomain( int maxLength)
    {
    maxLength_ = maxLength;
    initLengthRange();
    setExcluded( null);
    }

  /**
   * Returns the maximum value for unbounded length ranges.
   */
  public int getMaxLength()
    {
    return maxLength_;
    }

  /**
   * Defines a constant length range for values in this domain.
   */
  public void setLengthRange( Integer length)
    {
    setLengthRange( new IntegerConstant( Optional.ofNullable( length).map( m -> Math.max( 0, m)).orElse( 0)));
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Integer min, Integer max)
    {
    IntegerDomain length = new IntegerDomain( getMaxLength());
    length.setRange(
      Optional.ofNullable( min).orElse( 0),
      Optional.ofNullable( max).orElse( getMaxLength()));
    setLengthRange( length);
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Range range)
    {
    if( range == null)
      {
      setLengthRange( null, null);
      }
    else if( range.isConstant())
      {
      setLengthRange( Integer.valueOf( range.getMin()));
      }
    else
      {
      Integer min =
        Optional.ofNullable( range.getMin())
        .map( Integer::valueOf)
        .map( i -> range.isMinExclusive()? i + 1 : i)
        .orElse( null);

      Integer max =
        Optional.ofNullable( range.getMax())
        .map( Integer::valueOf)
        .map( i -> range.isMaxExclusive()? i - 1 : i)
        .orElse( null);
      
      setLengthRange( min, max);
      }
    }

  /**
   * Defines the initial length range for values in this domain.
   */
  protected void initLengthRange()
    {
    setLengthRange( null, null);
    }

  /**
   * Changes the length range for values in this domain.
   */
  protected void setLengthRange( ValueDomain<Integer> domain)
    {
    lengthRange_ = domain;
    }

  /**
   * Returns the length range for values in this domain.
   */
  protected ValueDomain<Integer> getLengthRange()
    {
    return lengthRange_;
    }

  /**
   * Changes the values excluded from this domain.
   */
  public void setExcluded( Set<T> excluded)
    {
    excluded_ = Optional.ofNullable( excluded).orElse( emptySet());
    }

  /**
   * Returns the values excluded from this domain.
   */
  public Set<T> getExcluded()
    {
    return excluded_;
    }

  /**
   * Changes the values excluded from this domain.
   */
  public abstract void setExcludedStrings( Set<String> excluded);

  /**
   * Returns the length of the given value.
   */
  protected abstract int getLength( T value);

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( Type.STRING);
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<DataValue<T>> values( Random random)
    {
    return
      candidates( random)
      .filter( value -> isNotExcluded( value, getExcluded()))
      .map( value -> dataValueOf( value));
    }
  
  /**
   * Returns a random sequence of possible members of this domain.
   */
  protected abstract Stream<T> candidates( Random random);

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( T value)
    {
    return
      // Length within min/max bounds?
      value != null && getLengthRange().contains( getLength( value))
      &&
      // Value not excluded?
      isNotExcluded( value, getExcluded());
    }

  /**
   * Returns true if <CODE>value</CODE> is not equal to of any of the <CODE>excluded</CODE> values.
   */
  protected boolean isNotExcluded( T value, Set<T> excluded)
    {
    return excluded.stream().allMatch( e -> !valuesEqual( value, e));
    }

  /**
   * Returns true if the given values are equal.
   */
  protected boolean valuesEqual( T value1, T value2)
    {
    return value1.equals( value2);
    }

  /**
   * Returns true if sequences in this domain are restricted to a constant length.  
   */
  protected boolean isRestrictedLength()
    {
    return false;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "length", getLengthRange())
      .toString();
    }

  private final int maxLength_;
  private ValueDomain<Integer> lengthRange_;
  private Set<T> excluded_;
  }
