//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.resolve.IntegerValue;

import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toSet;
import static org.cornutum.tcases.resolve.DataValue.Type;

/**
 * Defines a set of Integer values that can be used by a request.
 */
public class IntegerDomain extends NumberDomain<Integer>
  {
  /**
   * Creates a new IntegerDomain instance.
   */
  public IntegerDomain()
    {
    this( Integer.MAX_VALUE / 2);
    }
  
  /**
   * Creates a new IntegerDomain instance.
   */
  public IntegerDomain( long maxRange)
    {
    super( Type.INTEGER, maxRange);
    }
  
  /**
   * Creates a new IntegerDomain instance.
   */
  public IntegerDomain( int min, int max)
    {
    this();
    setRange( min, max);
    }
  
  /**
   * Creates a new IntegerDomain instance.
   */
  public IntegerDomain( Range range)
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
      .map( Integer::valueOf)
      .collect( toSet()));

    setRange(
      Optional.ofNullable( ifRange.map( Range::getMin).orElse( null))
      .map( Integer::valueOf)
      .map( i -> ifRange.map( Range::isMinExclusive).orElse( false)? i + 1 : i)
      .orElse( (int) -getMaxRange()),

      Optional.ofNullable( ifRange.map( Range::getMax).orElse( null))
      .map( Integer::valueOf)
      .map( i -> ifRange.map( Range::isMaxExclusive).orElse( false)? i - 1 : i)
      .orElse( (int) getMaxRange()));
    }

  /**
   * Defines the value range for this domain.
   */
  @Override
  public void setRange( Integer min, Integer max)
    {
    if( Integer.signum( min) != Integer.signum( max) && -Math.max( min, -Integer.MAX_VALUE) > Integer.MAX_VALUE - max)
      {
      throw new IllegalArgumentException( String.format( "Range=[%s,%s] exceeds maximum size=%s", min, max, Integer.MAX_VALUE));
      }
    super.setRange( min, max);
    }

  /**
   * If non-null, all values in this domain are a multiple of the given value.
   */
  @Override
  public void setMultipleOf( String multipleOf)
    {
    setMultipleOf( Optional.ofNullable( multipleOf).map( Integer::valueOf).orElse( (Integer) null));
    }

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  @Override
  public void setNotMultipleOfs( String[] notMultipleOfs)
    {
    setNotMultipleOfs(
      Arrays.stream( notMultipleOfs)
      .map( Integer::valueOf)
      .collect( toSet()));
    }

  /**
   * Returns true if <CODE>value</CODE> is a multiple of <CODE>multiple</CODE>.
   */
  @Override
  protected boolean isMultipleOf( Integer value, Integer multiple)
    {
    return value.intValue() % multiple.intValue() == 0;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  @Override
  public Stream<DataValue<Integer>> values( ResolverContext context)
    {
    // Find smallest and largest multiples in range
    int multiple = Optional.ofNullable( getMultipleOf()).orElse( 1);
    int firstMultiple = (int) Math.ceil( getMin().doubleValue() / multiple) * multiple;
    int lastMultiple = getMax() / multiple * multiple;
    
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
    
    int multiplesCount = lastMultiple/multiple - firstMultiple/multiple;
    multiplesCount += multiplesCount < Integer.MAX_VALUE? 1 : 0;

    final int originMultiple = firstMultiple;
    
    Stream<Integer> integers =
      multiplesCount < 1?
      Stream.empty() :

      multiplesCount == 1?
      Stream.of( firstMultiple) :

      context.getRandom().ints( 0, multiplesCount)
      .map( i -> originMultiple + i * multiple)
      .filter( i -> isNotExcluded( i, getExcluded()))
      .filter( i -> isNotMultipleOf( i, getNotMultipleOfs()))
      .mapToObj( Integer::new);

    return integers.map( this::dataValueOf);
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
