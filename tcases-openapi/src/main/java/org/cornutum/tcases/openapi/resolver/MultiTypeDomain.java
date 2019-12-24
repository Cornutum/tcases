//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Defines the set of all values of a given list of types.
 */
public class MultiTypeDomain implements ValueDomain<Object>
  {
  /**
   * Creates a new MultiTypeDomain instance.
   */
  public MultiTypeDomain( Type[] types)
    {
    types_ = types;
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<Object> values( Random random)
    {
    return Stream.generate( () -> valueOfType( random, types_[ random.nextInt( types_.length)]));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( Object value)
    {
    return Arrays.stream( types_).anyMatch( type -> hasType( value, type));
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return types_;
    }

  /**
   * Returns a value of the given type.
   */
  private Object valueOfType( Random random, Type type)
    {
    return
      type == Type.ARRAY?
      IntStream.range( 0, types_.length)
      .mapToObj( i -> random.nextInt())
      .collect( toList()) :

      type == Type.BOOLEAN?
      new Boolean( random.nextBoolean()) :

      type == Type.INTEGER?
      new Integer( random.nextInt()) :

      type == Type.NUMBER?
      new BigDecimal( (double) random.nextFloat() * random.nextInt()) :

      type == Type.OBJECT?
      Arrays.stream( types_)
      .filter( t -> t != Type.OBJECT)
      .collect(
        toMap(
          t -> t.toString().toLowerCase(),
          t -> valueOfType( random, t))) :

      type == Type.STRING?
      Long.toHexString( random.nextLong()) :

      null;      
    }

  /**
   * Returns true if the given value matches the given type.
   */
  private boolean hasType( Object value, Type type)
    {
    return
      type == Type.ARRAY?
      value instanceof List :

      type == Type.BOOLEAN?
      value instanceof Boolean :

      type == Type.INTEGER?
      value instanceof Integer || value instanceof Long :

      type == Type.NUMBER?
      value instanceof Number :

      type == Type.OBJECT?
      value instanceof Map :

      type == Type.STRING?
      value instanceof String :

      false; 
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getTypes())
      .toString();
    }

  private final Type[] types_;
  }
