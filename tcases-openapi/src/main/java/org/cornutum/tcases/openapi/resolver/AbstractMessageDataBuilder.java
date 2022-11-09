//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.resolve.DataValues.*;

import java.math.BigDecimal;
import java.util.Arrays;

import org.cornutum.tcases.resolve.DataValue;

/**
 * Base class for {@link MessageData} builders
 */
@SuppressWarnings("unchecked")
public abstract class AbstractMessageDataBuilder<T extends AbstractMessageDataBuilder<T>>
  {
  public T mediaType( String mediaType)
    {
    mediaType_ = mediaType;
    return (T) this;
    }
  
  public T valid( boolean valid)
    {
    valid_ = valid;
    return (T) this;
    }

  public MessageData build()
    {
    return new MessageData( value_, mediaType_, valid_);
    }

  public T arrayData( DataValue<?>... itemValues)
    {
    value_ = arrayOfAny( Arrays.asList( itemValues));
    return (T) this;
    }

  public T binaryData( byte[] value)
    {
    value_ = valueOf( value);
    return (T) this;
    }

  public T booleanData( boolean value)
    {
    value_ = valueOf( value);
    return (T) this;
    }

  public T decimalData( BigDecimal value)
    {
    value_ = valueOf( value);
    return (T) this;
    }

  public T integerData( int value)
    {
    value_ = valueOf( value);
    return (T) this;
    }

  public T longData( long value)
    {
    value_ = valueOf( value);
    return (T) this;
    }

  public T nullData()
    {
    value_ = nullValue();
    return (T) this;
    }

  public T objectData( ObjectValueBuilder builder)
    {
    value_ = builder.build();
    return (T) this;
    }

  public T stringData( String value)
    {
    value_ = stringOf( value);
    return (T) this;
    }

  public T base64Data( String value)
    {
    value_ = base64Of( value);
    return (T) this;
    }

  public T dateTimeData( String value)
    {
    value_ = dateTimeOf( value);
    return (T) this;
    }

  public T dateData( String value)
    {
    value_ = dateOf( value);
    return (T) this;
    }

  public T emailData( String value)
    {
    value_ = emailOf( value);
    return (T) this;
    }

  public T uuidData( String value)
    {
    value_ = uuidOf( value);
    return (T) this;
    }
  
  private DataValue<?> value_;
  private String mediaType_;
  private boolean valid_ = true;
  }
