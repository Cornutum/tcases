//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Stream;

/**
 * Defines an array value set.
 */
public class ArrayDomain<T> implements ValueDomain<List<T>>
  {
  /**
   * Creates a new ArrayDomain instance.
   */
  protected ArrayDomain( Map<String,Object> propertyValues)
    {
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<List<T>> values( Random random)
    {
    return null;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( List<T> value)
    {
    return true;
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( Type.ARRAY);
    }
  }
