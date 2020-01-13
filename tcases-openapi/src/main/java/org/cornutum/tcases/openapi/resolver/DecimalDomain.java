//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Arrays;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Defines a set of BigDecimal values that can be used by a request.
 */
public class DecimalDomain extends NumberDomain<BigDecimal>
  {
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain()
    {
    this( MAX_SIZE.longValue() / 2);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( long maxRange)
    {
    super( Type.NUMBER, maxRange);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( BigDecimal min, BigDecimal max)
    {
    this();
    setRange( min, max);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( double min, double max)
    {
    this( new BigDecimal( min), new BigDecimal( max));
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( Range range)
    {
    this();
    setRange( range);
    }

  /**
   * Defines the value range for this domain.
   */
  public void setRange( Range range)
    {
    setExcluded(
      range.getExcluded().stream()
      .map( BigDecimal::new)
      .collect( toSet()));

    BigDecimal min = 
      Optional.ofNullable( range.getMin())
      .map( BigDecimal::new)
      .orElse( new BigDecimal( -getMaxRange()));

    BigDecimal max = 
      Optional.ofNullable( range.getMax())
      .map( BigDecimal::new)
      .orElse( new BigDecimal( getMaxRange()));

    int unitScale = Math.max( 1, Math.max( min.scale(), max.scale()));
    BigDecimal unit = new BigDecimal( BigInteger.ONE, unitScale);
    
    setRange(
      range.isMinExclusive()? min.add( unit) : min,
      range.isMaxExclusive()? max.subtract( unit) : max);
    }

  /**
   * Defines the value range for this domain.
   */
  public void setRange( BigDecimal min, BigDecimal max)
    {
    if( max.subtract( min).compareTo( MAX_SIZE) > 0)
      {
      throw new IllegalArgumentException( String.format( "Range=[%s,%s] exceeds maximum size=%s", min, max, MAX_SIZE));
      }
    super.setRange( min, max);
    }

  /**
   * If non-null, all values in this domain are a multiple of the given value.
   */
  public void setMultipleOf( String multipleOf)
    {
    setMultipleOf( Optional.ofNullable( multipleOf).map( BigDecimal::new).orElse( (BigDecimal) null));
    }

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  public void setNotMultipleOfs( String[] notMultipleOfs)
    {
    setNotMultipleOfs(
      Arrays.stream( notMultipleOfs)
      .map( BigDecimal::new)
      .collect( toSet()));
    }

  /**
   * Returns true if <CODE>value</CODE> is a multiple of <CODE>multiple</CODE>.
   */
  protected boolean isMultipleOf( BigDecimal value, BigDecimal multiple)
    {
    return value.remainder( multiple).compareTo( BigDecimal.ZERO) == 0;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<BigDecimal> values( Random random)
    {
    // Find smallest and largest (multiples) in range
    int unitScale =
      Math.max(
        Math.max( 1, Optional.ofNullable( getMultipleOf()).map( BigDecimal::scale).orElse( 0)),
        Math.max( getMin().scale(), getMax().scale()));
    
    BigDecimal unit = new BigDecimal( BigInteger.ONE, unitScale);
    BigDecimal multiple = Optional.ofNullable( getMultipleOf()).orElse( unit);
    BigDecimal firstMultiple = divideCeiling( getMin(), multiple).multiply( multiple);
    BigDecimal lastMultiple = divideFloor( getMax(), multiple).multiply( multiple);

    // Find smallest fully-satisfying multiple in range
    for( ;

         firstMultiple.compareTo( lastMultiple) <= 0
           && !(isNotMultipleOf( firstMultiple, getNotMultipleOfs()) && isNotExcluded( firstMultiple, getExcluded()));

         firstMultiple = firstMultiple.add( multiple));

    // Find largest fully-satisfying multiple in range
    for( ;

         firstMultiple.compareTo( lastMultiple) <= 0
           && !(isNotMultipleOf( lastMultiple, getNotMultipleOfs()) && isNotExcluded( lastMultiple, getExcluded()));

         lastMultiple = lastMultiple.subtract( multiple));
    
    long multiplesCount = divideFloor( lastMultiple.subtract( firstMultiple), multiple).add( BigDecimal.ONE).min( MAX_SIZE).longValue();
    final BigDecimal originMultiple = firstMultiple;
    
    return
      multiplesCount < 1?
      Stream.empty() :

      multiplesCount == 1?
      Stream.of( firstMultiple) :

      random.longs( 0, multiplesCount)
      .mapToObj( i -> originMultiple.add( multiple.multiply( new BigDecimal(i))))
      .filter( d -> isNotExcluded( d, getExcluded()))
      .filter( d -> isNotMultipleOf( d, getNotMultipleOfs()));
    }

  /**
   * Returns the smallest integral value that is greater than or equal to <CODE>value / divisor</CODE>.
   */
  private BigDecimal divideCeiling( BigDecimal value, BigDecimal divisor)
    {
    BigDecimal[] quotient = value.divideAndRemainder( divisor);

    return
      quotient[1].compareTo( BigDecimal.ZERO) == 0
      ? quotient[0]
      : quotient[0].add( BigDecimal.ONE);
    }

  /**
   * Returns the largest integral value that is less than or equal to <CODE>value / divisor</CODE>.
   */
  private BigDecimal divideFloor( BigDecimal value, BigDecimal divisor)
    {
    return value.divideToIntegralValue( divisor);
    }

  private static final BigDecimal MAX_SIZE = new BigDecimal( Long.MAX_VALUE);
  }
