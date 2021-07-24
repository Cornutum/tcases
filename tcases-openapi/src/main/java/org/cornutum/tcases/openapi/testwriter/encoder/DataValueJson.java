//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import java.util.Objects;

import org.cornutum.tcases.openapi.resolver.DataValue;

/**
 * Converts a DataValue into a JSON string.
 */
public class DataValueJson implements DataValueConverter<String>
  {
  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  public static String toJson( DataValue<?> value)
    {
    return new DataValueJson().convert( value);
    }

  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  @Override
  public String convert( DataValue<?> value)
    {
    return Objects.toString( convertJsonValue_.convert( value), "");
    }

  private DataValueJsonValue convertJsonValue_ = new DataValueJsonValue(); 
  }
