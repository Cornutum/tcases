//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated email address value for a request case.
 */
public class EmailValue extends StringValue
  {
  /**
   * Creates a new EmailValue instance.
   */
  public EmailValue( String value)
    {
    super( value, "email");
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
