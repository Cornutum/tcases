//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

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

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getValue())
      .toString();
    }
  }
