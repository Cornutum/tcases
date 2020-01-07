//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.util.Map;
import java.util.Random;
import java.util.stream.Stream;

/**
 * Defines an object value set.
 */
public class ObjectDomain implements ValueDomain<Map<String,Object>>
  {
  /**
   * Creates a new ObjectDomain instance.
   */
  protected ObjectDomain( Map<String,Object> propertyValues)
    {
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<Map<String,Object>> values( Random random)
    {
    return Stream.empty();
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( Map<String,Object> value)
    {
    return true;
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( Type.OBJECT);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
