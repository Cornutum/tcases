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

/**
 * Defines a set of Long values that can be used by a request.
 */
public class LongDomain extends NumberDomain<Long>
  {
  /**
   * Creates a new LongDomain instance.
   */
  public LongDomain()
    {
    this( Long.MAX_VALUE / 2);
    }
  
  /**
   * Creates a new LongDomain instance.
   */
  public LongDomain( long maxRange)
    {
    super( Type.INTEGER, maxRange);
    }

  /**
   * Defines the value range for this domain.
   */
  public void setRange( Range range)
    {
    setExcluded(
      range.getExcluded().stream()
      .map( Long::valueOf)
      .collect( toSet()));

    setRange(
      Optional.ofNullable( getExcluded().isEmpty()? range.getMin() : null)
      .map( Long::valueOf)
      .orElse( -getMaxRange()),

      Optional.ofNullable( getExcluded().isEmpty()? range.getMax() : null)
      .map( Long::valueOf)
      .orElse( getMaxRange()));
    }

  /**
   * Defines the value range for this domain.
   */
  public void setRange( Long min, Long max)
    {
    if( Long.signum( min) != Long.signum( max) && -min > Long.MAX_VALUE - max)
      {
      throw new IllegalArgumentException( String.format( "Range=[%s,%s] exceeds maximum size=%s", min, max, Long.MAX_VALUE));
      }
    super.setRange( min, max);
    }

  /**
   * If non-null, all values in this domain are a multiple of the given value.
   */
  public void setMultipleOf( String multipleOf)
    {
    setMultipleOf( Optional.ofNullable( multipleOf).map( Long::valueOf).orElse( (Long) null));
    }

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  public void setNotMultipleOfs( String[] notMultipleOfs)
    {
    setNotMultipleOfs(
      Arrays.stream( notMultipleOfs)
      .map( Long::valueOf)
      .collect( toSet()));
    }

  /**
   * Returns true if <CODE>value</CODE> is a multiple of <CODE>multiple</CODE>.
   */
  protected boolean isMultipleOf( Long value, Long multiple)
    {
    return value.longValue() % multiple.longValue() == 0;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<Long> values( Random random)
    {
    // Find smallest and largest multiples in range
    long multiple = Optional.ofNullable( getMultipleOf()).orElse( 1L);
    long firstMultiple = (long) Math.ceil( (getMin().doubleValue() + 1) / multiple) * multiple;;
    long lastMultiple = (getMax() - 1) / multiple * multiple;;
    
    // Find smallest fully-satisfying multiple in range
    for( ;

         firstMultiple <= lastMultiple
           && !(isNotMultipleOf( firstMultiple, getNotMultipleOfs()) && isNotExcluded( firstMultiple, getExcluded()));

         firstMultiple += multiple);

    // Find largest fully-satisfying multiple in range
    for( ;

         firstMultiple <= lastMultiple
           && !(isNotMultipleOf( lastMultiple, getNotMultipleOfs()) && isNotExcluded( lastMultiple, getExcluded()));

         lastMultiple -= multiple);
    
    long multiplesCount = lastMultiple/multiple - firstMultiple/multiple + 1;
    final long originMultiple = firstMultiple;
    
    return
      multiplesCount < 1?
      Stream.empty() :

      multiplesCount == 1?
      Stream.of( firstMultiple) :

      random.longs( 0, multiplesCount)
      .map( i -> originMultiple + i * multiple)
      .filter( i -> isNotExcluded( i, getExcluded()))
      .filter( i -> isNotMultipleOf( i, getNotMultipleOfs()))
      .mapToObj( Long::new);
    }
  }
