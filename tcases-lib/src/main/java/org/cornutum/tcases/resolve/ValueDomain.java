//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValue.Type;

import java.util.Optional;
import java.util.stream.Stream;


/**
 * Defines a set of values that can be used by a request.
 */
public interface ValueDomain<T>
  {  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<DataValue<T>> values( ResolverContext context);

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( T value);

  /**
   * Returns true if the given object belongs to this domain.
   */
  @SuppressWarnings("unchecked")
  default public boolean containsObject( Object object)
    {
    try
      {
      T value =
        object == null
        ? null
        : (T) object;

      return contains( value);
      }
    catch( Exception e)
      {
      return false;
      }
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  default public boolean contains( DataValue<?> value)
    {
    return containsObject( Optional.ofNullable( value).map( DataValue::getValue).orElse( null));
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  public String getFormat();

  /**
   * Returns the type(s) of values that belong to this domain.
   */
  public Type[] getTypes();

  /**
   * If this domain contains only values of a single type, returns that type. Otherwise, returns null.
   */
  default public Type getType()
    {
    return
      Optional.of( getTypes())
      .filter( types -> types.length == 1)
      .map( types -> types[0])
      .orElse( null);
    }

  /**
   * Returns a random value from this domain.
   */
  default public DataValue<T> select( ResolverContext context)
    {
    return
      values( context)
      .findFirst()
      .orElseThrow( () -> new IllegalStateException( String.format( "Domain=%s is empty", this)));
    }

  /**
   * Returns a random value from this domain.
   */
  default public T selectValue( ResolverContext context)
    {
    return select( context).getValue();
    }

  /**
   * Returns a new {@link ArrayDomain} for arrays containing items in this domain.
   */
  default public ArrayDomain<T> arrayOf( int maxItems, boolean itemsUnique)
    {
    ArrayDomain<T> arrayDomain = new ArrayDomain<T>( maxItems);
    arrayDomain.setItemValues( this);
    arrayDomain.setItemsUnique( itemsUnique);
    return arrayDomain;
    }

  /**
   * Returns a new {@link ArrayDomain} for arrays containing items in this domain.
   */
  default public ArrayDomain<T> arrayOf( int maxItems)
    {
    return arrayOf( maxItems, false);
    }

  /**
   * Returns a new {@link ArrayDomain} for arrays containing items in this domain.
   */
  default public ArrayDomain<T> arrayOf()
    {
    return arrayOf( 16);
    }
}
