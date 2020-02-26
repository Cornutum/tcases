//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.Random;
import java.util.stream.Stream;

/**
 * Defines a singleton null value set.
 */
public class NullDomain extends AbstractValueDomain<Object>
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
  public Stream<DataValue<Object>> values( Random random)
    {
    return Stream.of( select( random));
    }

  /**
   * Returns a random value from this domain.
   */
  public DataValue<Object> select( Random random)
    {
    return dataValueOf( null);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<Object> dataValueOf( Object value)
    {
    return new NullValue();
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
