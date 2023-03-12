//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.resolve.NumberDomain.Range;
import org.cornutum.tcases.util.ToString;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;
import static org.cornutum.tcases.resolve.DataValue.Type;

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
    setOtherItemValues( null);
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
   * Changes the value domain for required array items.
   */
  public void setItemValues( ValueDomain<T> itemValues)
    {
    itemValues_ = itemValues;
    }

  /**
   * Returns the value domain for required array items.
   */
  public ValueDomain<T> getItemValues()
    {
    return itemValues_;
    }

  /**
   * Changes the value domain for additional array items.
   */
  @SuppressWarnings("unchecked")
  public void setOtherItemValues( ValueDomain<?> otherItemValues)
    {
    otherItemValues_ = (ValueDomain<T>) otherItemValues;
    }

  /**
   * Returns the value domain for additional array items.
   */
  public ValueDomain<T> getOtherItemValues()
    {
    return otherItemValues_;
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
  @Override
  public Stream<DataValue<List<DataValue<T>>>> values( ResolverContext context)
    {
    return Stream.generate( () -> dataValueOf( newArray( context)));
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<List<DataValue<T>>> dataValueOf( List<DataValue<T>> value)
    {
    return new ArrayValue<T>( value, getFormat());
    }

  /**
   * Returns a new random array from this domain.
   */
  private List<DataValue<T>> newArray( ResolverContext context)
    {
    List<DataValue<T>> items = new ArrayList<DataValue<T>>();
    int itemCount = getItemCount().selectValue( context);
    if( itemCount > 0)
      {
      // Select array items, observing any uniqueness constraint
      ValueDomain<T> nextItemValues = getItemValues();
      ValueDomain<T> otherItemValues = Optional.ofNullable( getOtherItemValues()).orElse( nextItemValues);
      
      boolean itemsUnique =
        Optional.of( isItemsUnique())
        .filter( unique -> unique)
        .map( unique -> {
            boolean otherItemsNull = Optional.ofNullable( otherItemValues.getType()).map( type -> type == Type.NULL).orElse( false);
            if( otherItemsNull)
              {
              context.warn( "Unable to generate unique non-null item objects -- all array items must be null");
              }
            return !otherItemsNull;
          })
        .orElse( false);

      while( items.size() < itemCount)
        {
        items.add( getNextItem( context, nextItemValues, itemCount, items, itemsUnique));
        nextItemValues = otherItemValues;
        }

      if( !itemsUnique && items.size() > 1)
        {
        // The first item, which satisfies required item assertions, must remain unchanged.
        // But given a different random target item ...
        int target = context.getRandom().nextInt( items.size() - 1) + 1;

        // ...and a different source item...
        int source;
        while( (source = context.getRandom().nextInt( items.size())) == target);

        // ...ensure target item is a duplicate of the source item.
        items.set( target, items.get( source));
        }
      }
    
    return items;
    }

  /**
   * Returns the random item for this array domain.
   */
  private DataValue<T> getNextItem( ResolverContext context, ValueDomain<T> nextItemValues, int itemCount, List<DataValue<T>> items, boolean itemsUnique)
    {
    return
      context.resultFor(
        String.format( "%sitem[%s] of %s", itemsUnique? "unique " : "", items.size(), itemCount),
        () -> context.tryUntil( () -> Optional.of( nextItemValues.select( context)).filter( item -> !( itemsUnique && items.contains( item)))));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  @Override
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
  @Override
  public Type[] getTypes()
    {
    return Type.only( Type.ARRAY);
    }

  @Override
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
  private ValueDomain<T> otherItemValues_;
  private boolean itemsUnique_;
  }
