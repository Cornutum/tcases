//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

/**
 * Defines an array value set.
 */
public class ArrayDomain<T> extends AbstractValueDomain<List<DataValue<T>>>
  {
  /**
   * Creates a new ArrayDomain instance.
   */
  public ArrayDomain()
    {
    this( 256);
    }
  
  /**
   * Creates a new ArrayDomain instance.
   */
  public ArrayDomain( int maxItems)
    {
    maxItems_ = maxItems;
    setItemCount( null, null);
    setItemValues( null);
    }

  /**
   * Returns the maximum number of items for an unbounded array.
   */
  public int getMaxItems()
    {
    return maxItems_;
    }

  /**
   * Defines a constant number of array items.
   */
  public void setItemCount( Integer itemCount)
    {
    setItemCount(
      new IntegerConstant(
        Optional.ofNullable( itemCount)
        .map( m -> Math.max( 0, m))
        .orElse( 0)));
    }

  /**
   * Defines the range for the number of array items.
   */
  public void setItemCount( Integer min, Integer max)
    {
    IntegerDomain itemCount = new IntegerDomain( getMaxItems());
    itemCount.setRange(
      Optional.ofNullable( min).orElse( 0),
      Optional.ofNullable( max).orElse( getMaxItems()));
    setItemCount( itemCount);
    }

  /**
   * Defines the range for the number of array items.
   */
  public void setItemCount( Range range)
    {
    if( range == null)
      {
      setItemCount( null, null);
      }
    else if( range.isConstant())
      {
      setItemCount( Integer.valueOf( range.getMin()));
      }
    else
      {
      Integer min =
        Optional.ofNullable( range.getMin())
        .map( Integer::valueOf)
        .map( i -> range.isMinExclusive()? i + 1 : i)
        .orElse( null);

      Integer max =
        Optional.ofNullable( range.getMax())
        .map( Integer::valueOf)
        .map( i -> range.isMaxExclusive()? i - 1 : i)
        .orElse( null);
      
      setItemCount( min, max);
      }
    }

  /**
   * Changes the range for the number of array items.
   */
  protected void setItemCount( ValueDomain<Integer> domain)
    {
    itemCount_ = domain;
    }

  /**
   * Returns the range for the number of array items.
   */
  protected ValueDomain<Integer> getItemCount()
    {
    return itemCount_;
    }

  /**
   * Changes the value domain for array items.
   */
  public void setItemValues( ValueDomain<T> itemValues)
    {
    itemValues_ = itemValues;
    }

  /**
   * Returns the value domain for array items.
   */
  public ValueDomain<T> getItemValues()
    {
    return itemValues_;
    }

  /**
   * Changes if array items are unique.
   */
  public void setItemsUnique( boolean unique)
    {
    itemsUnique_ = unique;
    }

  /**
   * Returns if array items are unique.
   */
  public boolean isItemsUnique()
    {
    return itemsUnique_;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<DataValue<List<DataValue<T>>>> values( ResolverOptions options)
    {
    return Stream.generate( () -> dataValueOf( newArray( options)));
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<List<DataValue<T>>> dataValueOf( List<DataValue<T>> value)
    {
    return new ArrayValue<T>( value);
    }

  /**
   * Returns a new random array from this domain.
   */
  private List<DataValue<T>> newArray( ResolverOptions options)
    {
    List<DataValue<T>> items;
    int itemCount;
    DataValue<T> nextItem;
    boolean itemsUnique = isItemsUnique();

    for( items = new ArrayList<DataValue<T>>(), itemCount = getItemCount().selectValue( options);
         items.size() < itemCount;
         items.add( nextItem))
      {
      for( nextItem = getItemValues().select( options);
           itemsUnique && items.contains( nextItem);
           nextItem = getItemValues().select( options));
      }

    if( !itemsUnique && itemCount > 1)
      {
      // Given a random target item...
      int target = options.getRandom().nextInt( itemCount);

      // ...and a different source item...
      int source;
      while( (source = options.getRandom().nextInt( itemCount)) == target);

      // ...ensure target item is a duplicate of the source item.
      items.set( target, items.get( source));
      }
    
    return items;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( List<DataValue<T>> value)
    {
    int size = value.size();

    return
      getItemCount().contains( size)
      && value.stream().allMatch( item -> getItemValues().contains( item))
      && (size < 2 || isItemsUnique() == (value.stream().collect( toSet()).size() == size));
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( Type.ARRAY);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getItemCount())
      .append( getItemValues())
      .toString();
    }

  private final int maxItems_;
  private ValueDomain<Integer> itemCount_;
  private ValueDomain<T> itemValues_;
  private boolean itemsUnique_;
  }
