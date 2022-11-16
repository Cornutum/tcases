//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.Characters;
import org.cornutum.tcases.util.ToString;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;
import static org.cornutum.tcases.resolve.DataValue.Type;

/**
 * Defines the set of values with any of a given list of types.
 */
public class MultiTypeDomain extends AbstractValueDomain<Object>
  {
  /**
   * Creates a new MultiTypeDomain instance.
   */
  public MultiTypeDomain( Type... types)
    {
    this( Characters.ANY, types);
    }
  
  /**
   * Creates a new MultiTypeDomain instance.
   */
  public MultiTypeDomain( Characters chars, Type... types)
    {
    chars_ = chars;
    
    typeDomains_ =
      Arrays.stream( types)
      .map( this::getValueDomain)
      .collect( toList());
    }

  /**
   * Returns the set of characters allowed in values for this domain.
   */
  public Characters getCharacters()
    {
    return chars_;
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  @Override
@SuppressWarnings("unchecked")
  public Stream<DataValue<Object>> values( ResolverContext context)
    {
    return
      Stream.generate( () -> {
        ValueDomain<?> domain = typeDomains_.get( context.getRandom().nextInt( typeDomains_.size()));
        String type = String.valueOf( domain.getTypes()[0]).toLowerCase();
        return context.resultFor( type, () -> (DataValue<Object>) domain.select( context));
        });
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<Object> dataValueOf( Object value)
    {
    throw new UnsupportedOperationException();
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  @Override
  public boolean contains( Object value)
    {
    return typeDomains_.stream().anyMatch( domain -> domain.containsObject( value));
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  @Override
  public Type[] getTypes()
    {
    return
      typeDomains_.stream()
      .map( domain -> domain.getTypes()[0])
      .toArray( Type[]::new);
    }

  /**
   * Returns a value domain of the given type.
   */
  private ValueDomain<?> getValueDomain( Type type)
    {
    return
      type == Type.ARRAY?
      getArrayDomain() :

      type == Type.BOOLEAN?
      getBooleanDomain() :

      type == Type.INTEGER?
      getIntegerDomain() :

      type == Type.NUMBER?
      getNumberDomain() :

      type == Type.OBJECT?
      getObjectDomain() :

      type == Type.STRING?
      getStringDomain() :

      null;      
    }

  /**
   * Returns the definition of array values in this domain.
   */
  private ValueDomain<?> getArrayDomain()
    {
    return getStringDomain().arrayOf( 3, true);
    }

  /**
   * Returns the definition of boolean values in this domain.
   */
  private ValueDomain<?> getBooleanDomain()
    {
    return new BooleanConstant( true);
    }

  /**
   * Returns the definition of integer values in this domain.
   */
  private ValueDomain<?> getIntegerDomain()
    {
    return new IntegerDomain( -1024, 1024);
    }

  /**
   * Returns the definition of number values in this domain.
   */
  private ValueDomain<?> getNumberDomain()
    {
    return new DecimalDomain( -1024.0, 1024.0);
    }

  /**
   * Returns the definition of object values in this domain.
   */
  private ValueDomain<?> getObjectDomain()
    {
    return
      new ObjectDomain(
        new IntegerDomain( 0, 3),
        new MultiTypeDomain( getCharacters(), Type.not( Type.OBJECT)),
        getCharacters());
    }

  /**
   * Returns the definition of string values in this domain.
   */
  private ValueDomain<?> getStringDomain()
    {
    return new AsciiStringDomain( 8, getCharacters());
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getTypes())
      .toString();
    }

  private final Characters chars_;
  private final List<ValueDomain<?>> typeDomains_;
  }
