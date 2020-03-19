//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * Represents a generated data value for a request case.
 */
public class DataValue<T>
  {
  public static enum Type
	{
	ARRAY, BOOLEAN, INTEGER, NULL, NUMBER, OBJECT, STRING;
	
	/**
	 * Returns the set containing only the specified type.
	 */
	public static Type[] only( Type type)
	  {
	  return new Type[]{ type };
	  }
	
	/**
	 * Returns the set of all non-null types except for the specified excluded type.
	 */
	public static Type[] not( Type... excluded)
	  {
	  return
	    Arrays.stream( Type.values())
	    .filter( type -> {
	      return
	        !type.equals( NULL)
	        && Arrays.stream( excluded).noneMatch( e -> type.equals( e) || (type.equals( INTEGER) && e == NUMBER));
	      })
	    .toArray( Type[]::new);
	  }
	
	/**
	 * Returns the set of all non-null types.
	 */
	public static Type[] any()
	  {
	  return not( NULL);
	  }
	}

  /**
   * Creates a new DataValue instance.
   */
  public DataValue( T value, Type type, String format)
    {
    value_ = value;
    type_ = type;
    format_ = format;
    }

  /**
   * Returns the data value.
   */
  public T getValue()
    {
    return value_;
    }
  
  /**
   * Returns the data type.
   */
  public Type getType()
    {
    return type_;
    }
  
  /**
   * Returns the data format.
   */
  public String getFormat()
    {
    return format_;
    }

  /**
   * Creates a new DataValue instance.
   */
  public static DataValue<Object> of( Object value, Type type, String format)
    {
    return new DataValue<Object>( value, type, format);
    }

  /**
   * Implements the Visitor pattern for this data value.
   */
  public void accept( DataValueVisitor visitor)
    {
    throw new UnsupportedOperationException( "Can't visit a generic DataValue");
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getValue())
      .toString();
    }

  public boolean equals( Object object)
    {
    @SuppressWarnings("unchecked")
	DataValue<T> other =
      Optional.ofNullable( object)
      .map( Object::getClass)
      .filter( otherClass -> getClass().isAssignableFrom( otherClass))
      .map( otherClass -> (DataValue<T>) object)
      .orElse( null);
      
    return
      other != null
      && Objects.equals( other.getValue(), getValue())
      && Objects.equals( other.getType(), getType())
      && Objects.equals( other.getFormat(), getFormat());
    }

  public int hashCode()
    {
    return
      DataValue.class.hashCode()
      ^ Objects.hashCode( getValue())
      ^ Objects.hashCode( getType())
      ^ Objects.hashCode( getFormat());
      
    }

  private final T value_;
  private final Type type_;
  private final String format_;
  }
