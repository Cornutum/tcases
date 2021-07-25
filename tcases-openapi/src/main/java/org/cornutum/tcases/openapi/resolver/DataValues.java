//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.toList;

/**
 * Factory methods for {@link DataValue} objects.
 */
public final class DataValues
  {
  /**
   * Creates a new DataValues instance.
   */
  private DataValues()
    {
    // Static methods only
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  @SafeVarargs
  public static <T> ArrayValue<T> arrayOf( DataValue<T>... items)
    {
    return new ArrayValue<T>( Arrays.asList( items));
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  @SuppressWarnings("unchecked")
  public static ArrayValue<Object>  arrayOfAny( List<DataValue<?>> items)
    {
	List<DataValue<Object>> objects = items.stream().map( item -> (DataValue<Object>) item).collect( toList());
    return new ArrayValue<Object>( objects);
    }

  /**
   * Returns a new {@link BinaryValue}.
   */
  public static BinaryValue valueOf( byte[] value)
    {
    return new BinaryValue( value);
    }

  /**
   * Returns a new {@link BooleanValue}.
   */
  public static BooleanValue valueOf( boolean value)
    {
    return new BooleanValue( value);
    }

  /**
   * Returns a new {@link DecimalValue}.
   */
  public static DecimalValue valueOf( BigDecimal value)
    {
    return new DecimalValue( value);
    }

  /**
   * Returns a new {@link IntegerValue}.
   */
  public static IntegerValue valueOf( int value)
    {
    return new IntegerValue( value);
    }

  /**
   * Returns a new {@link LongValue}.
   */
  public static LongValue valueOf( long value)
    {
    return new LongValue( value);
    }

  public static NullValue nullValue()
    {
    return new NullValue();
    }

  public static NullValue noValue()
    {
    return nullValue();
    }

  public static ObjectValueBuilder object()
    {
    return new ObjectValueBuilder();
    }

  /**
   * Returns a new {@link StringValue}.
   */
  public static StringValue stringOf( String value)
    {
    return new StringValue( value);
    }

  /**
   * Returns a new {@link Base64Value}.
   */
  public static Base64Value base64Of( String value)
    {
    return new Base64Value( value);
    }

  /**
   * Returns a new {@link DateTimeValue}.
   */
  public static DateTimeValue dateTimeOf( String value)
    {
    return new DateTimeValue( value);
    }

  /**
   * Returns a new {@link DateValue}.
   */
  public static DateValue dateOf( String value)
    {
    return new DateValue( value);
    }

  /**
   * Returns a new {@link EmailValue}.
   */
  public static EmailValue emailOf( String value)
    {
    return new EmailValue( value);
    }

  /**
   * Returns a new {@link UuidValue}.
   */
  public static UuidValue uuidOf( String value)
    {
    return new UuidValue( value);
    }

  public static class ObjectValueBuilder
    {
    public ObjectValueBuilder with( String name, DataValue<?> value)
      {
      object_.put( name, value);
      return this;
      }

    public ObjectValue build()
      {
      return new ObjectValue( object_);
      }
    
    private Map<String,DataValue<?>> object_ = new LinkedHashMap<String,DataValue<?>>();
    }

  }
