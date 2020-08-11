//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import static org.cornutum.tcases.openapi.OpenApiUtils.stringFormatMax;
import static org.cornutum.tcases.openapi.resolver.UuidConstant.isUuid;

import static org.apache.commons.lang3.StringUtils.leftPad;
import static org.apache.commons.lang3.StringUtils.rightPad;

import java.util.Arrays;

/**
 * Defines a set of UUID string values that can be used by a request.
 */
public class UuidDomain extends AbstractStringDomain
  {
  /**
   * Creates a new UuidDomain instance.
   */
  public UuidDomain()
    {
    super( MAX_LENGTH + 1, Characters.ASCII);
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new UuidValue( value);
    }

  /**
   * Defines the initial length range for values in this domain.
   */
  protected void initLengthRange()
    {
    setLengthRange( MAX_LENGTH);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return
      super.contains( value)
      && isUuid( value);
    }

  /**
   * Returns a new random string of the given length for this domain.
   *
   * @see java.util.UUID#randomUUID
   */
  protected String newValue( ResolverContext context, int length)
    {
    byte[] randomBytes = new byte[16];
    context.getRandom().nextBytes(randomBytes);
    randomBytes[6]  &= 0x0f;  /* clear version        */
    randomBytes[6]  |= 0x40;  /* set to version 4     */
    randomBytes[8]  &= 0x3f;  /* clear variant        */
    randomBytes[8]  |= 0x80;  /* set to IETF variant  */
    
    String value =
      String.format(
        "%s-%s-%s-%s-%s",
        toHex( randomBytes, 0, 4),
        toHex( randomBytes, 4, 2),
        toHex( randomBytes, 6, 2),
        toHex( randomBytes, 8, 2),
        toHex( randomBytes, 10, 6));

    // If invalid length, ensure UUID value is invalid.
    return
      length < value.length()?
      value.substring( 0, length) :

      length > value.length()?
      rightPad( value, length, '0')
      
      : value;
    }

  /**
   * Returns true if the given values are equal.
   */
  protected boolean valuesEqual( String value1, String value2)
    {
    return value1.equalsIgnoreCase( value2);
    }

  private static String toHex( byte[] bytes, int start, int length)
    {
    byte[] component = Arrays.copyOfRange( bytes, start, start + length);
    StringBuilder hex = new StringBuilder();
    for( byte b : component)
      {
      String hexByte = leftPad( Long.toHexString( b), 2, '0');
      hex.append( hexByte.substring( hexByte.length() - 2, hexByte.length()));
      }

    return hex.toString();
    }

  private static final int MAX_LENGTH = stringFormatMax( "uuid");
}
