//////////////////////////////////////////////////////////////////////////////
// 
//                    copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Objects;
import java.util.stream.Stream;

/**
 * Defines a singleton value set.
 */
public abstract class ConstantDomain<T> extends AbstractValueDomain<T>
  {
  /**
   * Creates a new ConstantDomain instance.
   */
  protected ConstantDomain( Type type, T value)
    {
    type_ = type;
    value_ = value;
    }

  /**
   * Returns the constant value for this domain.
   */
  public T getValue()
    {
    return value_;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  @Override
public Stream<DataValue<T>> values( ResolverContext context)
    {
    return Stream.of( dataValueOf( value_));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  @Override
public boolean contains( T value)
    {
    return Objects.equals( value, value_);
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  @Override
public Type[] getTypes()
    {
    return Type.only( type_);
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getValue())
      .toString();
    }

  private final T value_;
  private final Type type_;
  }
