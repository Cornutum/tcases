//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Represents a generated time string value for a request case.
 */
public class DateTimeValue extends StringValue
  {
  /**
   * Creates a new DateTimeValue instance.
   */
  public DateTimeValue( String value)
    {
    super( value, "date-time");
    }
  }
