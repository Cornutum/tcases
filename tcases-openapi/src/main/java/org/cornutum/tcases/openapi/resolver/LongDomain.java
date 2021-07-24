//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.Collections.emptySet;
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
   * Creates a new LongDomain instance.
   */
  public LongDomain( long min, long max)
    {
    this();
    setRange( min, max);
    }
  
  /**
   * Creates a new LongDomain instance.
   */
  public LongDomain( Range range)
    {
    this();
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
      .map( Long::valueOf)
      .collect( toSet()));

    setRange(
      Optional.ofNullable( ifRange.map( Range::getMin).orElse( null))
      .map( Long::valueOf)
      .map( i -> ifRange.map( Range::isMinExclusive).orElse( false)? i + 1 : i)
      .orElse( -getMaxRange()),

      Optional.ofNullable( ifRange.map( Range::getMax).orElse( null))
      .map( Long::valueOf)
      .map( i -> ifRange.map( Range::isMaxExclusive).orElse( false)? i - 1 : i)
      .orElse( getMaxRange()));
    }

  /**
   * Defines the value range for this domain.
   */
  @Override
  public void setRange( Long min, Long max)
    {
    if( Long.signum( min) != Long.signum( max) && -Math.max( min, -Long.MAX_VALUE) > Long.MAX_VALUE - max)
      {
      throw new IllegalArgumentException( String.format( "Range=[%s,%s] exceeds maximum size=%s", min, max, Long.MAX_VALUE));
      }
    super.setRange( min, max);
    }

  /**
   * If non-null, all values in this domain are a multiple of the given value.
   */
  @Override
  public void setMultipleOf( String multipleOf)
    {
    setMultipleOf( Optional.ofNullable( multipleOf).map( Long::valueOf).orElse( (Long) null));
    }

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  @Override
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
  @Override
  protected boolean isMultipleOf( Long value, Long multiple)
    {
    return value.longValue() % multiple.longValue() == 0;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  @Override
  public Stream<DataValue<Long>> values( ResolverContext context)
    {
    // Find smallest and largest multiples in range
    long multiple = Optional.ofNullable( getMultipleOf()).orElse( 1L);
    long firstMultiple = (long) Math.ceil( getMin().doubleValue() / multiple) * multiple;
    long lastMultiple = getMax() / multiple * multiple;
    
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
    
    long multiplesCount = lastMultiple/multiple - firstMultiple/multiple;
    multiplesCount += multiplesCount < Long.MAX_VALUE? 1 : 0;

    final long originMultiple = firstMultiple;
    
    Stream<Long> longs =
      multiplesCount < 1?
      Stream.empty() :

      multiplesCount == 1?
      Stream.of( firstMultiple) :

      context.getRandom().longs( 0, multiplesCount)
      .map( i -> originMultiple + i * multiple)
      .filter( i -> isNotExcluded( i, getExcluded()))
      .filter( i -> isNotMultipleOf( i, getNotMultipleOfs()))
      .mapToObj( Long::new);

    return longs.map( this::dataValueOf);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<Long> dataValueOf( Long value)
    {
    return new LongValue( value);
    }
  }
