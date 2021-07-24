//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.DataValue;

import java.util.Objects;
import java.util.Optional;
import javax.json.JsonString;
import javax.json.JsonValue.ValueType;
import javax.json.JsonValue;

/**
 * Converts a DataValue into a plain text string. For values of type ARRAY or OBJECT,
 * this produces the same result as {@link DataValueJson}.
 */
public class DataValueText implements DataValueConverter<String>
  {
  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  public static String toText( DataValue<?> value)
    {
    return new DataValueText().convert( value);
    }

  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  @Override
  public String convert( DataValue<?> value)
    {
    JsonValue jsonValue = convertJsonValue_.convert( value);
    ValueType type = Optional.ofNullable( jsonValue).map( JsonValue::getValueType).orElse( null);

    return
      type == ValueType.STRING
      ? ((JsonString) jsonValue).getString()
      : Objects.toString( jsonValue, "");
    }

  private DataValueJsonValue convertJsonValue_ = new DataValueJsonValue(); 
  }
