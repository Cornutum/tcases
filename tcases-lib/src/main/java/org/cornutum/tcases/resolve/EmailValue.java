//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

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
  }
