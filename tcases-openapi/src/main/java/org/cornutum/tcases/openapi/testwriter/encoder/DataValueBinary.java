//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.BinaryValue;
import org.cornutum.tcases.openapi.resolver.DataValue;

/**
 * Converts a DataValue into a byte array.
 */
public class DataValueBinary implements DataValueConverter<byte[]>
  {
  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  public static byte[] toBytes( DataValue<?> value)
    {
    return new DataValueBinary().convert( value);
    }

  /**
   * Returns the converted form of the given {@link DataValue}.
   */
  @Override
public byte[] convert( DataValue<?> value)
    {
    try
      {
      return
        value == null?
        new byte[0] :

        value.getClass().equals( BinaryValue.class)?
        ((BinaryValue) value).getValue() :

        convertText_.convert( value).getBytes( "UTF-8");
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( String.format( "Can't byte array for %s", value), e);
      }
    }

  private DataValueText convertText_ = new DataValueText(); 
  }
