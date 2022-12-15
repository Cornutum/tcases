//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated date string value for a request case.
 */
public class DateValue extends StringValue
  {
  /**
   * Creates a new DateValue instance.
   */
  public DateValue( String value)
    {
    super( value, "date");
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
