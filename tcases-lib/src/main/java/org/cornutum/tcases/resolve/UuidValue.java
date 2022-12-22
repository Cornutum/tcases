//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated UUID string value for a test case.
 */
public class UuidValue extends StringValue
  {
  /**
   * Creates a new UuidValue instance.
   */
  public UuidValue( String value)
    {
    super( value, "uuid");
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
