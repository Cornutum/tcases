//////////////////////////////////////////////////////////////////////////////
// 
//                    copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Defines an enumerated value set.
 */
public abstract class EnumDomain<T> extends AbstractValueDomain<T>
  {
  /**
   * Creates a new EnumDomain instance.
   */
  protected EnumDomain( Type type, Iterable<String> enums)
    {
    this( type);
    setEnums( enums);
    }
  
  /**
   * Creates a new EnumDomain instance.
   */
  protected EnumDomain( Type type, Collection<T> enums)
    {
    this( type);
    setEnums( enums.stream());
    }
  
  /**
   * Creates a new EnumDomain instance.
   */
  private EnumDomain( Type type)
    {
    type_ = type;
    }

  /**
   * Returns the enumerated values for this domain.
   */
  public Iterable<T> getEnums()
    {
    return enums_;
    }

  /**
   * Changes the enumerated values for this domain.
   */
  private void setEnums( Iterable<String> enums)
    {
    setEnums( toStream( enums).map( this::valueOf));
    }

  /**
   * Changes the enumerated values for this domain.
   */
  private void setEnums( Stream<T> enums)
    {
    enums_ = enums.distinct().collect( toList());
    }

  /**
   * Returns the value represented by the given string.
   */
  protected abstract T valueOf( String value);

  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<DataValue<T>> values( ResolverContext context)
    {
    return Stream.generate( () -> dataValueOf( enums_.get( context.getRandom().nextInt( enums_.size()))));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( T value)
    {
    return enums_.contains( value);
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
      .append( getEnums())
      .toString();
    }

  private final Type type_;
  private List<T> enums_;
  }
