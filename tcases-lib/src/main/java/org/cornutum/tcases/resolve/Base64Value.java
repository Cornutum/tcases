//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ToString;

/**
 * Represents a generated base64 address value for a test case.
 */
public class Base64Value extends StringValue
  {
  /**
   * Creates a new Base64Value instance.
   */
  public Base64Value( String value)
    {
    super( value, "byte");
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
