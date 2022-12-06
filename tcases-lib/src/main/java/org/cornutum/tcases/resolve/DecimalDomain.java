//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toSet;

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
    this( null);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( String format)
    {
    this( MAX_SIZE.longValue() / 2, format);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( long maxRange)
    {
    this( maxRange, null);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( long maxRange, String format)
    {
    super( Type.NUMBER, maxRange);
    format_ = format;
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( BigDecimal min, BigDecimal max)
    {
    this( min, max, null);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( BigDecimal min, BigDecimal max, String format)
    {
    this( format);
    setRange( min, max);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( double min, double max)
    {
    this( min, max, null);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( double min, double max, String format)
    {
    this( new BigDecimal( min), new BigDecimal( max), format);
    }
  
  /**
   * Creates a new DecimalDomain instance.
   */
  public DecimalDomain( Range range, String format)
    {
    this( format);
    setRange( range);
    }

  /**
   * Defines the value range for this domain.
   */
  @Override
  public void setRange( Range range)
    {
    Optional<Range> ifRange = Optional.ofNullable( range);

    setExcluded(
      ifRange.map( Range::getExcluded).orElse( emptySet())
      .stream()
      .map( BigDecimal::new)
      .collect( toSet()));

    BigDecimal min = 
      Optional.ofNullable( ifRange.map( Range::getMin).orElse( null))
      .map( BigDecimal::new)
      .orElse( new BigDecimal( -getMaxRange()));

    BigDecimal max = 
      Optional.ofNullable( ifRange.map( Range::getMax).orElse( null))
      .map( BigDecimal::new)
      .orElse( new BigDecimal( getMaxRange()));

    int unitScale = Math.max( 1, Math.max( min.scale(), max.scale()));
    BigDecimal unit = new BigDecimal( BigInteger.ONE, unitScale);
    
    setRange(
      ifRange.map( Range::isMinExclusive).orElse( false)? min.add( unit) : min,
      ifRange.map( Range::isMaxExclusive).orElse( false)? max.subtract( unit) : max);
    }

  /**
   * Defines the value range for this domain.
   */
  @Override
  public void setRange( BigDecimal min, BigDecimal max)
    {
    min = Optional.ofNullable( min).orElse( new BigDecimal( -getMaxRange()));
    max = Optional.ofNullable( max).orElse( new BigDecimal( getMaxRange()));

    if( max.subtract( min).compareTo( MAX_SIZE) > 0)
      {
      throw new IllegalArgumentException( String.format( "Range=[%s,%s] exceeds maximum size=%s", min, max, MAX_SIZE));
      }

    super.setRange( min, max);
    }

  /**
   * If non-null, all values in this domain are a multiple of the given value.
   */
  @Override
  public void setMultipleOf( String multipleOf)
    {
    setMultipleOf( Optional.ofNullable( multipleOf).map( BigDecimal::new).orElse( (BigDecimal) null));
    }

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  @Override
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
  @Override
  protected boolean isMultipleOf( BigDecimal value, BigDecimal multiple)
    {
    return value.remainder( multiple).compareTo( BigDecimal.ZERO) == 0;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  @Override
  public Stream<DataValue<BigDecimal>> values( ResolverContext context)
    {
    // Find unit of iteration over value range
    int unitScale =
      Math.max(
        Math.max( 1, Optional.ofNullable( getMultipleOf()).map( BigDecimal::scale).orElse( 0)),
        Math.max( getMin().scale(), getMax().scale()));
    
    BigDecimal unit;
    for( unit = new BigDecimal( BigInteger.ONE, unitScale);
         getNotMultipleOfs().contains( unit);
         unit = new BigDecimal( BigInteger.ONE, ++unitScale));

    // Find smallest and largest (multiples) in range
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
    
    Stream<BigDecimal> decimals =
      multiplesCount < 1?
      Stream.empty() :

      multiplesCount == 1?
      Stream.of( firstMultiple) :

      context.getRandom().longs( 0, multiplesCount)
      .mapToObj( i -> originMultiple.add( multiple.multiply( new BigDecimal(i))))
      .filter( d -> isNotExcluded( d, getExcluded()))
      .filter( d -> isNotMultipleOf( d, getNotMultipleOfs()));

    return decimals.map( this::dataValueOf);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<BigDecimal> dataValueOf( BigDecimal value)
    {
    return new DecimalValue( value, format_);
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

  private final String format_;
  
  private static final BigDecimal MAX_SIZE = new BigDecimal( Long.MAX_VALUE);
  }
