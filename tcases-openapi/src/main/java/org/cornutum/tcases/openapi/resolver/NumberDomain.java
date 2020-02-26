//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toSet;
import static java.util.stream.Collectors.joining;

/**
 * Defines a set of numeric values that can be used by a request.
 */
public abstract class NumberDomain<T extends Number & Comparable<T>> extends AbstractValueDomain<T>
  {
  /**
   * Creates a new NumberDomain instance.
   */
  protected NumberDomain( Type type, long maxRange)
    {
    type_ = type;
    maxRange_ = maxRange;
    setNotMultipleOfs( (Set<T>) null);
    setExcluded( null);
    }

  /**
   * Returns the maximum magnitude for unbounded value ranges.
   */
  public long getMaxRange()
    {
    return maxRange_;
    }

  /**
   * Defines the value range for this domain.
   */
  public abstract void setRange( Range range);

  /**
   * If non-null, all values in this domain are a multiple of the given value.
   */
  public abstract void setMultipleOf( String multipleOf);

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  public abstract void setNotMultipleOfs( String[] notMultipleOfs);

  /**
   * Returns true if <CODE>value</CODE> is a multiple of <CODE>multiple</CODE>.
   */
  protected abstract boolean isMultipleOf( T value, T multiple);

  /**
   * Changes the (inclusive) minimum and the (inclusive) maximum for this domain.
   */
  public void setRange( T min, T max)
    {
    min_ = min;
    max_ = max;
    }

  /**
   * Returns the (inclusive) minimum for this domain.
   */
  public T getMin()
    {
    return min_;
    }

  /**
   * Returns the (inclusive) maximum for this domain.
   */
  public T getMax()
    {
    return max_;
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
   * Changes the factor required for all values in this domain.
   */
  public void setMultipleOf( T multipleOf)
    {
    multipleOf_ = multipleOf;
    }

  /**
   * Returns the factor required for all values in this domain.
   */
  public T getMultipleOf()
    {
    return multipleOf_;
    }

  /**
   * Changes the factors not allowed for any values in this domain.
   */
  public void setNotMultipleOfs( Set<T> notMultipleOfs)
    {
    notMultipleOfs_ = 
      notMultipleOfs == null
      ? new HashSet<T>()
      : notMultipleOfs;
    }

  /**
   * Returns the factors not allowed for any values in this domain.
   */
  public Set<T> getNotMultipleOfs()
    {
    return notMultipleOfs_;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( T value)
    {
    return
      // Value within min/max bounds?
      (value != null && value.compareTo( getMin()) >= 0  && value.compareTo( getMax()) <= 0)
      &&
      // Value not excluded?
      isNotExcluded( value, getExcluded())
      &&
      // Value satisfies any multiple-of constraint?
      Optional.ofNullable( getMultipleOf()).map( m -> isMultipleOf( value, m)).orElse( true)
      &&
      // Value satisfies all not-multiple-of constraints?
      isNotMultipleOf( value, getNotMultipleOfs());
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( type_);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getMin())
      .append( getMax())
      .toString();
    }

  /**
   * Returns true if <CODE>value</CODE> is a not a multiple of any of the <CODE>multiples</CODE>.
   */
  protected boolean isNotMultipleOf( T value, Set<T> multiples)
    {
    return multiples.stream().allMatch( m -> !isMultipleOf( value, m));
    }

  /**
   * Returns true if <CODE>value</CODE> is not equal to of any of the <CODE>excluded</CODE> values.
   */
  protected boolean isNotExcluded( T value, Set<T> excluded)
    {
    return excluded.stream().allMatch( e -> value.compareTo( e) != 0);
    }

  /**
   * Represents a range of numeric values.
   */
  public static class Range
    {
    /**
     * Creates a new Range instance.
     */
    private Range( String min, boolean minExclusive, String max, boolean maxExclusive, Set<String> excluded)
      {
      min_ = min;
      minExclusive_ = minExclusive;
      max_ = max;
      maxExclusive_ = maxExclusive;
      excluded_ = excluded;
      }

    /**
     * Returns the minimum of this range.
     */
    public String getMin()
      {
      return min_;
      }

    /**
     * Returns true if the minimum is exclusive.
     */
    public boolean isMinExclusive()
      {
      return minExclusive_;
      }

    /**
     * Returns the maximum of this range.
     */
    public String getMax()
      {
      return max_;
      }

    /**
     * Returns true if the maximum is exclusive.
     */
    public boolean isMaxExclusive()
      {
      return maxExclusive_;
      }

    /**
     * Returns the values excluded from this range.
     */
    public Set<String> getExcluded()
      {
      return excluded_;
      }

    /**
     * Return true if this range contains a single constant value.
     */
    public boolean isConstant()
      {
      return min_ != null && min_.equals( max_);
      }

    /**
     * Returns the Range represented by the given number binding.
     */
    public static Range of( String op, String bound)
      {
      return of( op, bound, emptySet());
      }

    /**
     * Returns the Range represented by the given number binding.
     */
    public static Range of( VarBinding binding)
      {
      String rangeDef = String.valueOf( binding.getValue());
      Matcher matcher = numberRangePattern_.matcher( rangeDef);
      if( !matcher.matches())
        {
        throw new RequestCaseException( String.format( "Invalid number range='%s'", rangeDef));
        }

      Set<String> excluded =
        Optional.ofNullable( binding.getAnnotationList( "excluded"))
        .map( values -> values.stream().collect( toSet()))
        .orElse( emptySet());

      String op = matcher.group(2);
      String bound = matcher.group(3);

      return of( op, bound, excluded);
      }

    /**
     * Returns the Range represented by the given number binding.
     */
    private static Range of( String op, String bound, Set<String> excluded)
      {
      String min =
        op == null || op.startsWith( ">")
        ? bound
        : null;

      String max =
        op == null || op.startsWith( "<")
        ? bound
        : null;

      boolean minExclusive = ">".equals( op);
      boolean maxExclusive = "<".equals( op);

      return new Range( min, minExclusive, max, maxExclusive, excluded);
      }

    public String toString()
      {
      Optional<Set<String>> excluded =
        getExcluded().isEmpty()
        ? Optional.empty()
        : Optional.of( getExcluded());
      
      return
        String.format(
          "%s%s,%s%s%s",
          isMinExclusive()? "(" : "[",
          String.valueOf( getMin()),
          String.valueOf( getMax()),
          isMaxExclusive()? ")" : "]",
          excluded.map( e -> String.format( "-[%s]", e.stream().collect( joining( ",")))).orElse( ""));
      }

    private final String min_;
    private final boolean minExclusive_;
    private final String max_;
    private final boolean maxExclusive_;
    private final Set<String> excluded_;
    }  

  private final Type type_;
  private final long maxRange_;
  private T min_;
  private T max_;
  private Set<T> excluded_;
  private T multipleOf_;
  private Set<T> notMultipleOfs_;

  private final static Pattern numberRangePattern_ = Pattern.compile( "(Other)|(?:([<>]=?) )?(.*)");
  }
