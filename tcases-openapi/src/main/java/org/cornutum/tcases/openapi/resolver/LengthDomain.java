//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Defines the range of possible lengths for a sequence value.
 */
public class LengthDomain extends AbstractValueDomain<Integer>
  {
  /**
   * Creates a new LengthDomain instance.
   */
  public LengthDomain( Integer length)
    {
    min_ = max_ = Optional.ofNullable( length).map( m -> Math.max( 0, m)).orElse( 0);
    valueDomain_ = new IntegerConstant( min_);
    }
  
  /**
   * Creates a new LengthDomain instance.
   */
  public LengthDomain( int min, int max)
    {
    min_ = min;
    max_ = max;
    valueDomain_ = new IntegerDomain( min, max);
    }

  /**
   * Returns the minimum value for this domain.
   */
  public int getMin()
    {
    return min_;
    }

  /**
   * Returns the maximum value for this domain.
   */
  public int getMax()
    {
    return max_;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<DataValue<Integer>> values( ResolverContext context)
    {
    return valueDomain_.values( context);
    }
  
  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( Integer value)
    {
    return valueDomain_.contains( value);
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return valueDomain_.getTypes();
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Integer> dataValueOf( Integer value)
    {
    return new IntegerValue( value);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getMin())
      .append( getMax())
      .toString();
    }

  private final ValueDomain<Integer> valueDomain_;
  private final int min_;
  private final int max_;
  }
