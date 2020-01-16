//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.util.Random;
import java.util.stream.Stream;

/**
 * Defines a singleton null value set.
 */
public class NullDomain implements ValueDomain<Object>
  {
  /**
   * Creates a new NullDomain instance.
   */
  public NullDomain()
    {
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<Object> values( Random random)
    {
    return Stream.builder().add( null).build();
    }

  /**
   * Returns a random value from this domain.
   */
  public Object select( Random random)
    {
    return null;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( Object value)
    {
    return value == null;
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( Type.NULL);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }
  }
