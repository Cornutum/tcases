//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

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
  public static ArrayValue<String> arrayOf( String... items)
    {
    return arrayOf( null, items);
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  public static ArrayValue<String> arrayOf( String format, String... items)
    {
    return
      new ArrayValue<String>(
        Arrays.stream( items)
        .map( item -> stringOf( format, item))
        .collect( toList()));
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  public static ArrayValue<Integer> arrayOf( Integer... items)
    {
    return
      new ArrayValue<Integer>(
        Arrays.stream( items)
        .map( item -> valueOf( item))
        .collect( toList()));
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  public static ArrayValue<Long> arrayOf( Long... items)
    {
    return
      new ArrayValue<Long>(
        Arrays.stream( items)
        .map( item -> valueOf( item))
        .collect( toList()));
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  public static ArrayValue<Boolean> arrayOf( Boolean... items)
    {
    return
      new ArrayValue<Boolean>(
        Arrays.stream( items)
        .map( item -> valueOf( item))
        .collect( toList()));
    }

  /**
   * Returns a new {@link ArrayValue}.
   */
  public static ArrayValue<BigDecimal> arrayOf( BigDecimal... items)
    {
    return
      new ArrayValue<BigDecimal>(
        Arrays.stream( items)
        .map( item -> valueOf( item))
        .collect( toList()));
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
   * Returns a new formatted {@link StringValue}.
   */
  public static StringValue stringOf( String format, String value)
    {
    return
      "byte".equals( format)?
      base64Of( value) :

      "date-time".equals( format)?
      dateTimeOf( value) :

      "date".equals( format)?
      dateOf( value) :

      "email".equals( format)?
      emailOf( value) :

      "uuid".equals( format)?
      uuidOf( value) :

      stringOf( value);
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

  /**
   * Returns a new {@link BigDecimal}.
   */
  public static BigDecimal bigDecimalOf( String value)
    {
    return new BigDecimal( value);
    }

  /**
   * Returns a new {@link BigDecimal}.
   */
  public static BigDecimal bigDecimalOf( DataValue<?> value)
    {
    BigDecimal decimal;

    if( value == null)
      {
      decimal = null;
      }
    else if( value.getClass().equals( IntegerValue.class))
      {
      decimal = new BigDecimal( ((IntegerValue) value).getValue());
      }
    else if( value.getClass().equals( LongValue.class))
      {
      decimal = new BigDecimal( ((LongValue) value).getValue());
      }
    else if( value.getClass().equals( DecimalValue.class))
      {
      decimal = ((DecimalValue) value).getValue();
      }
    else
      {
      throw new IllegalArgumentException( String.format( "Expected numeric value but found %s", value));
      }

    return decimal;
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
