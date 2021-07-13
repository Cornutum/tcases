//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Optional;

/**
 * Represents a generated string value for a request case.
 */
public class StringValue extends DataValue<String>
  {
  /**
   * Creates a new StringValue instance.
   */
  public StringValue( String value)
    {
    this( value, null);
    }
  
  /**
   * Creates a new StringValue instance.
   */
  public StringValue( String value, String format)
    {
    super( value, Type.STRING, format);
    }

  /**
   * Implements the Visitor pattern for this data value.
   */
  @Override
public void accept( DataValueVisitor visitor)
    {
    visitor.visit( this);
    }

  @Override
public String toString()
    {
    ToStringBuilder builder =
      ToString.getBuilder( this)
      .append( getValue());

    Optional.ofNullable( getFormat())
      .ifPresent( format -> builder.append( format));

    return builder.toString();
    }
  }
